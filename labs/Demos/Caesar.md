# Caesar Cipher

## Installation and Loading

To use Cryptol, you must first [install](../../INSTALL.md) it. To 
load this document into Cryptol, change to your `cryptol-course` 
directory in a terminal (Linux) or command prompt (Windows), then 
run Cryptol via a locally installed binary or Docker image:

### Linux

```sh
.../cryptol-course> cryptol labs/Demos/Caesar.md
...

Loading module Cryptol
Loading module labs::Demos::Caesar
labs::Demos::Caesar> 
```

Alternatively, you can use the `:m` or `:l` command from within 
Cryptol to load this document. (To avoid errors, make sure Cryptol 
was started in the `cryptol-course` directory). 

```sh
.../cryptol-course> cryptol
...

Loading module Cryptol
Cryptol> :m labs::Demos::Caesar
Loading module labs::Demos::Caesar
labs::Demos::Caesar> 
```

### Windows

```sh
.../cryptol-course> cryptol labs/Demos/Caesar.md
...

Loading module Cryptol
Loading module labs::Demos::Caesar
labs::Demos::Caesar> 
```

Alternatively, you can use the `:m` or `:l` command from within 
Cryptol to load this document. (To avoid errors, make sure Cryptol 
was started in the `cryptol-course` directory). 

```sh
...\cryptol-course> cryptol
...

Loading module Cryptol
Cryptol> :m labs::Demos::Caesar
Loading module labs::Demos::Caesar
labs::Demos::Caesar> 
```

### Docker (Linux)
```sh
.../cryptol-course> docker run --read-only --mount type=bind,src=$(pwd),dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it cryptolcourse/cryptol
Loading module Cryptol
Cryptol> :m labs::Demos::Caesar
Loading module labs::Demos::Caesar
labs::Demos::Caesar> 
```

### Docker (Windows)
```sh
...\cryptol-course> docker run --read-only --mount type=bind,src=%CD%,dst=/mnt/cryptol-course --env CRYPTOLPATH=/mnt/cryptol-course -it cryptolcourse/cryptol
Loading module Cryptol
Cryptol> :m labs::Demos::Caesar
Loading module labs::Demos::Caesar
labs::Demos::Caesar> 
```

## Overview

The [Caesar cipher](https://en.wikipedia.org/wiki/Caesar_cipher) is a 
simple shift cipher named after Julius Caesar. This cipher was...
evidently effective against illiterate adversaries of ancient Rome, 
but will it protect its secrets from you and Cryptol...?

```
/* Simple Caesar cipher */
module labs::Demos::Caesar where
```

A Caesar cipher simply rotates characters in the alphabet by a fixed 
amount. (Caesar himself used a left rotation by 3, according to 
Suetonius.) We could define this simply and directly in Cryptol:

```
caesar msg = map rot3 msg
  where
    rot3 c =
      if 'A' <= c /\ c <= 'Z' then
        (['A'..'Z':Char] >>> 3) @ (c - 'A')
      else
        c
```

```sh
labs::Demos::Caesar> :s ascii=on
labs::Demos::Caesar> caesar "ATTACK AT DAWN"
"XQQXZH XQ AXTK"
```

...and that's it! Thank you for trying the Caesar demo...

...


## Generalization

OK, we can explore this a little further. Let's generalize it:

```
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

The above definition assumed a continguous character set, which does 
not necessarily hold in general. So we'll need a function to find a 
character in the alphabet:

```
/**
 * index (from end) of first occurence (from start) of item `x` in 
 * sequence `L`
 */
index L x = if (or M) then (lg2 ((0b0 # M) + 1) - 1) else (length M)
  where
    M = (map ((==) x) L)
```

It's...not obvious how this function works, so let's establish a 
property to verify its correctness:

```
/** `index` is correct for any sequence */
indexCorrect L x = elem x L ==> L ! (index L x) == x

/** index is correctly identified for all characters in alphabet */
property charIsAtIndex = indexCorrect alphabet
```

```sh
labs::Demos::Caesar> :prove charIsAtIndex
Q.E.D.
(Total Elapsed Time: 0.072s, using "Z3")
```

The property even holds for other sequences, repeating or not:

```sh
labs::Demos::Caesar> :prove \(A : [64]Char) -> indexCorrect A
Q.E.D.
(Total Elapsed Time: 1.172s, using "Z3")
labs::Demos::Caesar> :prove \(L : [33]Integer) -> indexCorrect L
Q.E.D.
(Total Elapsed Time: 0.347s, using "Z3")
```

But to work for a Caesar cipher, each character in the alphabet needs 
to be unique.


## Encryption via Alphabet Rotation

Our encryption strategy will mimic the Romans in preselecting a key 
and generating a cipher alphabet, then generating cipher characters 
by matching position:

```
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

```sh
labs::Demos::Caesar> :s ascii=off
labs::Demos::Caesar> :s base=2
labs::Demos::Caesar> 0b11001010 <<< 4
0b10101100
```

...but over sequences of arbitrary shape...

```sh
labs::Demos::Caesar> :s base=10
labs::Demos::Caesar> [1, 2, 3, 4, 5 : Integer] <<< 3
[4, 5, 1, 2, 3]
labs::Demos::Caesar> :s ascii=on
labs::Demos::Caesar> "RACECAR " <<< 4
"CAR RACE"
labs::Demos::Caesar> :t (<<<)
(<<<) : {n, ix, a} (fin n, fin ix) => [n]a -> [ix] -> [n]a
```


## Decryption

To decrypt, simply swap the plaintext and cipher alphabets in the 
search and output operations:

```
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

```
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

```sh
labs::Demos::Caesar> :check v1
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::Demos::Caesar> :check v2
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::Demos::Caesar> :check v3
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Properties

In addition to test vectors, we would like to know whether decryption 
with the same key recovers the original plaintext for all messages:

```
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

```sh
labs::Demos::Caesar> :prove recovery_4
Q.E.D.
(Total Elapsed Time: 4.226s, using "Z3")
labs::Demos::Caesar> :prove recovery_14
Q.E.D.
(Total Elapsed Time: 28.246s, using "Z3")
```


## Security Analysis

So is this a good cipher?  Well, no.  Let's...here.  We can manually 
deduce a key from known ciphertext...

```sh
labs::Demos::Caesar> map (\k -> (k, decrypt k "SXQW SJLXK FJB J SRWPUNQNRVNA BLQVRMC")) [0..25]
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
solving...

```sh
labs::Demos::Caesar> :sat \k -> encrypt k "ILLUMINATI CONFIRMED" == "NQQZRNSFYN HTSKNWRJI"
(\k -> encrypt k "ILLUMINATI CONFIRMED" == "NQQZRNSFYN HTSKNWRJI")
  21 = True
(Total Elapsed Time: 0.089s, using "Z3")
```


## Conclusion

You came, you saw, and you conquered the Caesar cipher in Cryptol.
Ave, Caesar!
