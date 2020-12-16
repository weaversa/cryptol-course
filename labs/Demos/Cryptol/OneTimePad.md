# Introduction

This demo gives an overview of a simple cryptographic operation, the
one-time pad.

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
Cryptol> :m labs::Demos::Cryptol::OneTimePad
Loading module Cryptol
Loading module Cryptol
Loading module labs::Demos::Cryptol::OneTimePad
```

We start by defining a new module for this lab:

```cryptol
module labs::Demos::Cryptol::OneTimePad where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.  
In general, you should run `Xcryptol session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# One-Time Pad

Cryptol's documentation includes the excellent [Programming Cryptol](
https://cryptol.net/files/ProgrammingCryptol.pdf), a comprehensive
introduction to Cryptol that offers a "crash course" in the
fundamentals before moving on to:

* a chapter on classic ciphers such as:
  + [Caesar](https://en.wikipedia.org/wiki/Caesar_cipher),
  + [Vigenère](https://en.wikipedia.org/wiki/Vigenère_cipher), and
  + [Atbash](https://en.wikipedia.org/wiki/Atbash),
+ a chapter on the [Enigma machine](
  https://en.wikipedia.org/wiki/Enigma_machine),
+ a chapter on proving simple properties about algorithms (by
  invoking Boolean satisfiability modulo theories, or "SMT",
  solvers), and
+ a chapter on the more modern [Advanced Encryption Standard](
  https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) (AES).

That sounds too complicated for now. Let's start with a motivating
example based on the [One-Time Pad](
https://en.wikipedia.org/wiki/One-time_pad), a simple theoretically
perfect but impractical technique in which plaintext is paired with a
[pre-shared key](https://en.wikipedia.org/wiki/Pre-shared_key) that
is at least as long as the plaintext and only used once.

## Example

Suppose Alice wishes to encipher the message `HELLO` using the
pre-shared key `ZUGESAGT` with [ASCII](https://ascii.cl) encoding and
the [XOR]( https://en.wikipedia.org/wiki/Exclusive_or) pairing
operation. Then she encodes `HELLO` as the ASCII hexadecimal sequence
`[0x48, 0x45, 0x4C, 0x4C, 0x4F]` and `ZUGES` (the letters needed from
the one-time pad to cover the plaintext) as `[0x5A, 0x55, 0x47, 0x45,
0x53]`. Pairing these yields the ciphertext `[0x48^0x5A, 0x45^0x55,
0x4C^0x47, 0x4C^0x45, 0x4F^0x53] == [0x12, 0x10, 0x0B, 0x09, 0x1C]`,
which she sends to Bob. Bob, also knowing the pre- shared key
`ZUGESAGT`, observes that the ciphertext `[0x12, 0x10, 0x0B, 0x09,
0x1C]` has length `5`, likewise encodes enough of the pre-shared key
(`ZUGES`) as `[0x5A, 0x55, 0x47, 0x45, 0x53]`, applies the reverse
pairing operation (which is also `XOR`), yielding the original
plaintext `[0x12^0x5A, 0x10^0x55, 0x0B^0x47, 0x09^0x45, 0x1C^0x53] ==
[0x48, 0x45, 0x4C, 0x4C, 0x4F]` (`"HELLO"`). Both parties must then
unfortunately discard `ZUGES` so attackers can't exploit it --
one-time pad and all that.

## Hello Cryptol

That was a pretty vague and long-winded bloviation about the humble
one-time pad. We could use Cryptol to express this algorithm clearly
and concisely (provided the audience can read the language).

```cryptol
/** Encrypt plaintext `pt` using pre-shared key `psk` */
encrypt :
    {k, m}
    (fin k, k >= m) =>
    (String k) -> (String m) -> (String m)
encrypt psk pt = ct
  where
    ct = (take psk) ^ pt

/** Decrypt plaintext; same as `encrypt` */
decrypt = encrypt

/**
 * Check test vector
 *   "ZUGESAGT" "HELLO" -> [0x12, 0x10, 0x0B, 0x09, 0x1C]
 */
property test =
    (encrypt "ZUGESAGT" "HELLO" == [0x12, 0x10, 0x0B, 0x09, 0x1C])

/**
 * Verify that for any pre-shared key `psk` and plaintext `pt` of
 * size 8 and 5, respectively, decrypting ciphertext encrypted
 * using the same pre-shared key yields the original plaintext
 */
property decrypt_of_encrypt_yields_original_plaintext_8_5 (psk, pt) =
    (decrypt psk (encrypt`{8, 5} psk pt)) == pt
```

## Setting and Reading Variables

Great. Now let's try applying the module to the previous example:

```cryptol
psk1 = "ZUGESAGT"
pt1 = "HELLO"
ct1 = (encrypt psk1 pt1)
```

OK, we've assigned variables representing the pre-shared key (`psk1`),
plaintext (`pt1`), and ciphertext (`ct1`) from the example. Let's see
the plaintext:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> pt1
[0x48, 0x45, 0x4c, 0x4c, 0x4f]
```

That wasn't what we assigned to `pt1`!!!  Actually it is, just shown
differently, in this case as a sequence of 5 hexadecimal bytes
rather than a string of 5 characters.  We can ask the interpreter
to show us a string instead:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :s ascii=on
labs::Demos::Cryptol::OneTimePad> pt1
"HELLO"
```

That was pleasant.  Now let's see the ciphertext:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> ct1
"\DC2\DLE\v\t\FS"
```

That looks ciphertexty, all right.  Entering `:s` shows all the
configuration settings:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :s
ascii = on
base = 16
core-lint = off
debug = off
infLength = 5
mono-binds = on
prover = z3
prover-stats = on
satNum = 1
smtfile = -
tc-debug = 0
tc-solver = z3 -smt2 -in
tests = 100
warnDefaulting = on
warnShadowing = on
```

## Help!!!

If a symbol's name isn't descriptive enough, we can use `:h` to display
help text for it:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :h encrypt

    encrypt : {k, m} (fin k, k >= m) =>
                String k -> String m -> String m

`(encrypt psk pt)` encrypts plaintext `pt` using pre-shared key `psk`.
```

## Function Evaluation

Cool. Let's do a quick sanity check:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> decrypt psk1 ct1
"HELLO"
```

## Previous Result

It matches! Our sanity is intact. Well, maybe not -- let's make
sure:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> it == pt1
True
```

## Test Vectors

Nice! So that one example checks out, as expressed by the `test`
property in our module. Let's prove it:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :prove test
Q.E.D.
(Total Elapsed Time: 0.028s, using Z3)
```

(For concrete test vectors like this one, `:check test` also fits the
bill and is marginally faster...but it's important to remember to do
this only for static test vectors and properties that `:prove` can't
finish in the time you are willing to wait.)

## Properties

That's just one test vector, but Cryptol is not just a test runner.
We can apply SMT solving to prove it for all cases (of a particular
type), as reflected in the
`decrypt_of_encrypt_yields_original_plaintext_8_5` property. That's a
rather sesquipedalian name; let's use tab-completion to prove the
property:

```Text
labs::Demos::Cryptol::OneTimePad> :prove dec<Tab>
labs::Demos::Cryptol::OneTimePad> :prove decrypt_<Tab>
labs::Demos::Cryptol::OneTimePad> :prove decrypt_of_encrypt_yields_original_plaintext_8_5<Enter>
Q.E.D.
(Total Elapsed Time: 0.028s, using Z3)
```

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :prove decrypt_of_encrypt_yields_original_plaintext_8_5
Q.E.D.
```

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :prove decrypt_of_encrypt_yields_original_plaintext_8_5
Q.E.D.
```

## Satisfiability

Good to know. Another use of SMT solving is to find an input
satisfying a property. We could apply this to our one-time pad by
demonstrating why using the one-time pad (pre-shared key) more than
one time is not a good idea. Suppose Bob replies to Alice with a
second plaintext/ciphertext exchange also using the pre-shared key
`ZUGESAGT`:

```cryptol
pt2 = "GOODBYE"
ct2 = (encrypt psk1 pt2)
```

Now suppose an attacker Eve doesn't know the pre-shared key, but has
compromised Bob's network port, sees a 7-character message, and
deduces that with no further communication the message might
reasonably have been "GOODBYE". Then Eve can exploit Cryptol to
deduce the pre-shared key Bob just used:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> :sat \psk2 -> (encrypt`{7} psk2 pt2) == ct2
Satisfiable
(\psk2 -> (encrypt`{7} psk2 pt2) == ct2) "ZUGESAG" = True
(Total Elapsed Time: 0.044s, using "Z3")
```

(That "`->`" syntax defines a [lambda function](
https://en.wikipedia.org/wiki/Anonymous_function) that maps one
argument (`psk2`) to an expression that will return `True` or `False`,
basically asking for a pre-shared key that encrypts the given
plaintext to the given ciphertext. Of course, Eve could have just
applied `XOR` directly; this example is silly.)

Now that Eve has the pre-shared key for this exchange, she could
stash `it` and try `it` (`it` is the counterexample produced by the 
last `:sat` command) on the message Bob received earlier:

```Xcryptol session
labs::Demos::Cryptol::OneTimePad> it
{result = True, arg1 = "ZUGESAG"}
labs::Demos::Cryptol::OneTimePad> :sat \pt3 -> (encrypt (it.arg1) pt3) == ct1
Satisfiable
(\pt3 -> (encrypt "ZUGESAG" pt3) == ct) "HELLO" = True
(Total Elapsed Time: 0.028s, using Z3)
```

Hello!!! So much for communications security...

# Conclusion

Well that was fun. We have clearly expressed the one-time pad using
Cryptol, demonstrated it on some test cases, verified some simple
properties, and manipulated it for nefarious misdeeds.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

Up: [Cryptol Demos](/Demos/Cryptol/Demos.md)
