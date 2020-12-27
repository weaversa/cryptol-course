# Introduction

This module defines a trivial transposition "cipher" that just 
reverses a message.  The cipher is defined in terms of the 
`labs::Transposition::Transposition` library, which defines `encrypt` 
and `decrypt` functions given a `PermutationMapping`, which in this 
case just returns ``reverse (take`{n} [0...])`` given a message of 
length `n`.

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed and
  * this module to load successfully.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter and
  * the `:prove` and `:sat` commands
  * ...

## Skills You'll Learn

Eh...not much.  This module will demonstrate module imports and 
sequence manipulation, but it's nothing a course participant 
hasn't seen better described before.  It does, however, offer a 
trivial example against which to compare more interesting 
transposition ciphers in later modules.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the 
Cryptol interpreter. Load this module from within the Cryptol 
interpreter running in the `cryptol-course` directory with:

```Xcryptol session
Cryptol> :m labs::Transposition::EsreverAnswers
Loading module Cryptol
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofsAnswers
Loading module labs::Transposition::CommonPropertiesAnswers
Loading module labs::Transposition::TranspositionAnswers
Loading module labs::Transposition::EsreverAnswers
```

We start by defining the module for this lab:

```cryptol
module labs::Transposition::EsreverAnswers where
```

Additionally, we will import the common transposition cipher 
definitions:

```cryptol
import labs::Transposition::TranspositionAnswers
```

# Esrever Encryption and Decryption

**EXERCISE**: Define a `PermutationMapping` `pi` such that 
`encrypt pi msg` will return `reverse msg`.  Do not use the value of 
`msg`.  Check it with `pi_test` and prove it using  `pi_correct`.

```cryptol
/** a permutation mapping that just reverses order */
pi: {n} (fin n) => [n][width n]
pi = reverse (take`{n} [0...])
```

```cryptol
property pi_test = and
    [ encrypt pi "" == ""
    , encrypt pi "A" == "A"
    , encrypt pi "STRESSED" == "DESSERTS"
    , encrypt pi "RACECAR" == "RACECAR"
    , encrypt pi "SEMORDNILAP" == "PALINDROMES"
    , decrypt pi "" == ""
    , decrypt pi "A" == "A"
    , decrypt pi "STRESSED" == "DESSERTS"
    , decrypt pi "RACECAR" == "RACECAR"
    , decrypt pi "SEMORDNILAP" == "PALINDROMES"
    ]

/** `encrypt pi` reverses sequence order */ 
pi_correct: {n, a} (fin n, Cmp a) => [n]a -> Bit
pi_correct msg = (encrypt pi) msg == reverse msg
```

```Xcryptol session
labs::Transposition::EsreverAnswers> :check pi_test
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::Transposition::EsreverAnswers> :prove pi_correct`{7, Char}
Q.E.D.
(Total Elapsed Time: 0.006s, using Z3)
labs::Transposition::EsreverAnswers> :prove pi_correct`{128, Char}
Q.E.D.
(Total Elapsed Time: 0.014s, using Z3)
labs::Transposition::EsreverAnswers> :check pi_correct`{4096, Char}
Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^32768 values)
```

**EXERCISE**: Define a predicate that `encrypt pi` is equivalent to 
`decrypt pi`, and prove it.

```cryptol
encrypt_decrypt_equiv: {n, a} (fin n, Cmp a) => [n]a -> Bit
encrypt_decrypt_equiv = encrypt pi === decrypt pi
```

```Xcryptol session
labs::Transposition::EsreverAnswers> :prove encrypt_decrypt_equiv`{32, Char}
Q.E.D.
(Total Elapsed Time: 0.008s, using Z3)
labs::Transposition::EsreverAnswers> :prove encrypt_decrypt_equiv`{512, Char}
Q.E.D.
(Total Elapsed Time: 0.053s, using Z3)
labs::Transposition::EsreverAnswers> :prove encrypt_decrypt_equiv`{4096, Char}
Q.E.D.
(Total Elapsed Time: 0.730s, using Z3)
```

# Conclusion

This lab presented a rudimentary transposition "cipher" that just 
reverses a message, to see how to apply concepts common to 
transposition ciphers.  The next labs will present somewhat more 
interesting ciphers.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!
||||
|-:|:-:|-|
|| [^ Transposition Ciphers](/labs/Transposition/Contents.md) ||
| [< Transposition](/labs/Transposition/Transposition.md) | **Esrever (Answers)** | [Scytale >](/labs/Transposition/Scytale.md) |
|| [? Esrever](/labs/Transposition/Esrever.md) ||
