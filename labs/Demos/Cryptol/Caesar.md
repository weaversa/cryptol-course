# Introduction

This demo gives an overview of the famous Caesar cipher.

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed and
  * this module to load successfully.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter and
  * the `:prove` and `:sat` commands.

## Skills You'll Learn

By the end of this demo you'll understand a bit more about the Cryptol
language and how to use the interpreter to prove properties or find
bugs in Cryptol specifications.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol session
Loading module Cryptol
Cryptol> :m labs::Demos::Cryptol::Caesar
Loading module Cryptol
Loading module labs::Demos::Cryptol::Caesar
```

The Cryptol module starts by defining a new module for this lab:

```cryptol
module labs::Demos::Cryptol::Caesar where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.  
In general, you should run `Xcryptol session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Caesar Cipher

The [Caesar cipher](https://en.wikipedia.org/wiki/Caesar_cipher) is a
simple shift cipher named after Julius Caesar. This cipher was...
evidently effective against illiterate adversaries of ancient Rome,
but will it protect its secrets from you and Cryptol...?

A Caesar cipher simply rotates characters in the alphabet by a fixed
amount. (Caesar himself used a left rotation by 3, according to
Suetonius.) We could define this simply and directly in Cryptol:

```cryptol
caesar msg = map rot3 msg
  where
    rot3 c =
      if 'A' <= c /\ c <= 'Z' then
        (['A'..'Z':Char] >>> 3) @ (c - 'A')
      else
        c
```

```Xcryptol session
labs::Demos::Cryptol::Caesar> :s prover=abc
labs::Demos::Cryptol::Caesar> :s ascii=on
labs::Demos::Cryptol::Caesar> caesar "ATTACK AT DAWN"
"XQQXZH XQ AXTK"
```

...and that's it! Thank you for trying the Caesar demo...

...


## Generalization

OK, we can explore this a little further. Let's generalize it:

```cryptol
/** number of characters in alphabet */
type w = 26

/** an alphabet of characters included in a rotation cipher */
type Alphabet = String w

/** a left-rotation amount */
type Key = [width w]

/** alphabet of characters to "encrypt" */
alphabet = ['A'..'Z'] : Alphabet
```


## Aside: A confounding search

The above definition assumed a contiguous character set, which does
not necessarily hold in general. So we'll need a function to find a
character in the alphabet:

```cryptol
/**
 * index (from end) of first occurrence (from start) of item `x` in
 * sequence `L`
 */
index:
    {n, a} 
    (fin n, Eq a) =>
    [n]a -> a -> [1 + n]
index L x = if (or M) then (lg2 ((0b0 # M) + 1) - 1) else (length M)
  where
    M = (map ((==) x) L)
```

It's...not obvious how this function works, so let's establish a
property to verify its correctness:

```cryptol
/** `index` is correct for any sequence */
indexCorrect:
    {n, a} 
    (fin n, Eq a) =>
    [n]a -> a -> Bit
indexCorrect L x = elem x L ==> L ! (index L x) == x

/** index is correctly identified for all characters in alphabet */
charIsAtIndex : Char -> Bit
property charIsAtIndex = indexCorrect alphabet
```

```Xcryptol session
labs::Demos::Cryptol::Caesar> :prove charIsAtIndex
Q.E.D.
(Total Elapsed Time: 0.072s, using "ABC")
```

The property even holds for other sequences, repeating or not:

```Xcryptol session
labs::Demos::Cryptol::Caesar> :prove \(A : [64]Char) -> indexCorrect A
Q.E.D.
(Total Elapsed Time: 1.172s, using "ABC")
labs::Demos::Cryptol::Caesar> :prove \(L : [33][32]) -> indexCorrect L
Q.E.D.
(Total Elapsed Time: 0.347s, using "ABC")
```

But to work for a Caesar cipher, each character in the alphabet needs
to be unique.


## Encryption via Alphabet Rotation

Our encryption strategy will mimic the Romans in preselecting a key
and generating a cipher alphabet, then generating cipher characters
by matching position:

```cryptol
encrypt : {n} Key -> String n -> String n
encrypt key msg = map rot msg
  where
    /* cipher alphabet */
    alphabet' = alphabet >>> key

    rot c = if (i < length alphabet) then (alphabet' ! i) else c
      where
        i = index alphabet c
```

Note the `alphabet >>> key` part: Cryptol allows rotation not only
over bit sequences, ...

```Xcryptol session
labs::Demos::Cryptol::Caesar> :s ascii=off
labs::Demos::Cryptol::Caesar> :s base=2
labs::Demos::Cryptol::Caesar> 0b11001010 <<< 4
0b10101100
```

...but over sequences of arbitrary shape...

```Xcryptol session
labs::Demos::Cryptol::Caesar> :s base=10
labs::Demos::Cryptol::Caesar> [1, 2, 3, 4, 5 : Integer] <<< 3
[4, 5, 1, 2, 3]
labs::Demos::Cryptol::Caesar> :s ascii=on
labs::Demos::Cryptol::Caesar> "RACECAR " <<< 4
"CAR RACE"
labs::Demos::Cryptol::Caesar> :t (<<<)
(<<<) : {n, ix, a} (fin n, Integral ix) => [n]a -> ix -> [n]a
```


## Decryption

To decrypt, simply swap the plaintext and cipher alphabets in the
search and output operations:

```cryptol
decrypt : {n} Key -> String n -> String n
decrypt key msg' = map rot msg'
  where
    /* cipher alphabet */
    alphabet' = alphabet >>> key

    rot c = if (i < length alphabet) then (alphabet ! i) else c
      where
        i = index alphabet' c
```


## Testing

For a sanity check, we can check some examples...

```cryptol
/** classic test vector */
property v1 = encrypt 3 "ATTACK AT DAWN" == "XQQXZH XQ AXTK"

/** Wikipedia test vector */
property v2 =
    encrypt 3 "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" ==
              "QEB NRFZH YOLTK CLU GRJMP LSBO QEB IXWV ALD"

/** ROT13 example */
property v3 = encrypt 13 "ABJURER" == "NOWHERE"
```

Though we could `:prove` these, for static test cases with no
variables, it makes more sense to `:check` these (this saves
the solver some work and often speeds up such tests).

```Xcryptol session
labs::Demos::Cryptol::Caesar> :check v1
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::Demos::Cryptol::Caesar> :check v2
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::Demos::Cryptol::Caesar> :check v3
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Properties

In addition to test vectors, we would like to know whether decryption
with the same key recovers the original plaintext for all messages:

```cryptol
/** Decryption with same key recovers original message */
recovery : {n} fin n => Key -> String n -> Bit
recovery key msg = decrypt key (encrypt key msg) == msg

/** Decrypting encrypted `msg` of length `1` returns original `msg`. */
property recovery_1 = recovery`{1}
/** Decrypting encrypted `msg` of length `2` returns original `msg`. */
property recovery_2 = recovery`{2}
/** Decrypting encrypted `msg` of length `3` returns original `msg`. */
property recovery_3 = recovery`{3}
/** Decrypting encrypted `msg` of length `4` returns original `msg`. */
property recovery_4 = recovery`{4}

/** Decrypting encrypted `msg` of length `14` returns original `msg`. */
property recovery_14 = recovery`{14}
```

```Xcryptol session
labs::Demos::Cryptol::Caesar> :prove recovery_4
Q.E.D.
(Total Elapsed Time: 0.245s, using "ABC")
labs::Demos::Cryptol::Caesar> :prove recovery_14
Q.E.D.
(Total Elapsed Time: 0.956s, using "ABC")
```


## Security Analysis

So is this a good cipher?  Well, no.  Let's...here.  We can manually
deduce a key from known ciphertext...

```Xcryptol session
labs::Demos::Cryptol::Caesar> map (\k -> (k, decrypt k "SXQW SJLXK FJB J SRWPUNQNRVNA BLQVRMC")) [0..25]
[(0, "SXQW SJLXK FJB J SRWPUNQNRVNA BLQVRMC"),
 (1, "TYRX TKMYL GKC K TSXQVOROSWOB CMRWSND"),
 (2, "UZSY ULNZM HLD L UTYRWPSPTXPC DNSXTOE"),
 (3, "VATZ VMOAN IME M VUZSXQTQUYQD EOTYUPF"),
 (4, "WBUA WNPBO JNF N WVATYRURVZRE FPUZVQG"),
 (5, "XCVB XOQCP KOG O XWBUZSVSWASF GQVAWRH"),
 (6, "YDWC YPRDQ LPH P YXCVATWTXBTG HRWBXSI"),
 (7, "ZEXD ZQSER MQI Q ZYDWBUXUYCUH ISXCYTJ"),
 (8, "AFYE ARTFS NRJ R AZEXCVYVZDVI JTYDZUK"),
 (9, "BGZF BSUGT OSK S BAFYDWZWAEWJ KUZEAVL"),
 (10, "CHAG CTVHU PTL T CBGZEXAXBFXK LVAFBWM"),
 (11, "DIBH DUWIV QUM U DCHAFYBYCGYL MWBGCXN"),
 (12, "EJCI EVXJW RVN V EDIBGZCZDHZM NXCHDYO"),
 (13, "FKDJ FWYKX SWO W FEJCHADAEIAN OYDIEZP"),
 (14, "GLEK GXZLY TXP X GFKDIBEBFJBO PZEJFAQ"),
 (15, "HMFL HYAMZ UYQ Y HGLEJCFCGKCP QAFKGBR"),
 (16, "INGM IZBNA VZR Z IHMFKDGDHLDQ RBGLHCS"),
 (17, "JOHN JACOB WAS A JINGLEHEIMER SCHMIDT"),
 (18, "KPIO KBDPC XBT B KJOHMFIFJNFS TDINJEU"),
 (19, "LQJP LCEQD YCU C LKPINGJGKOGT UEJOKFV"),
 (20, "MRKQ MDFRE ZDV D MLQJOHKHLPHU VFKPLGW"),
 (21, "NSLR NEGSF AEW E NMRKPILIMQIV WGLQMHX"),
 (22, "OTMS OFHTG BFX F ONSLQJMJNRJW XHMRNIY"),
 (23, "PUNT PGIUH CGY G POTMRKNKOSKX YINSOJZ"),
 (24, "QVOU QHJVI DHZ H QPUNSLOLPTLY ZJOTPKA"),
 (25, "RWPV RIKWJ EIA I RQVOTMPMQUMZ AKPUQLB")]
```

...and we can recover a key from chosen plaintext through SAT
solving...here we switch solvers back to `z3` because `abc` sometimes
has troubles with this one.

```Xcryptol session
labs::Demos::Cryptol::Caesar> :s prover=z3
labs::Demos::Cryptol::Caesar> :sat \k -> encrypt k "ILLUMINATI CONFIRMED" == "NQQZRNSFYN HTSKNWRJI"
Satisfiable
(\k -> encrypt k "ILLUMINATI CONFIRMED" == "NQQZRNSFYN HTSKNWRJI")
  21 = True
(Total Elapsed Time: 0.089s, using "ABC")
```

# Conclusion

You came, you saw, and you conquered the Caesar cipher in Cryptol.
Ave, Caesar!

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!
||||
|-:|:-:|-|
|| [^ Cryptol Demos](/labs/Demos/Cryptol/Demos.md) ||
|| **Caesar** | [One-Time Pad >](/labs/Demos/Cryptol/OneTimePad.md) |
