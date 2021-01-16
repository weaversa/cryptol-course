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

```Xcryptol-session
Cryptol> :m labs::Transposition::Esrever
Loading module Cryptol
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofs
Loading module labs::Transposition::CommonProperties
Loading module labs::Transposition::Transposition
Loading module labs::Transposition::Esrever
```

We start by defining the module for this lab:

```cryptol
module labs::Transposition::Esrever where
```

Additionally, we will import the common transposition cipher 
definitions:

```cryptol
import labs::Transposition::Transposition
```

# Esrever Encryption and Decryption

**EXERCISE**: Define a `PermutationMapping` `pi` such that 
`encrypt pi msg` will return `reverse msg`.  Do not use the value of 
`msg`.  Check it with `pi_test` and prove it using  `pi_correct`.

```cryptol
/** a permutation mapping that just reverses order */
pi: {n} [n][width n]
pi = undefined
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

**EXERCISE**: Define a predicate that `encrypt pi` is equivalent to 
`decrypt pi`, and prove it.

```cryptol
encrypt_decrypt_equiv: {n, a} [n]a -> Bit
encrypt_decrypt_equiv = undefined
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
|| [ ^ Transposition Ciphers](./Contents.md) ||
| [< Transposition](./Transposition.md) | **Esrever** | [Scytale >](./Scytale.md) |
|| [! Esrever (Answers)](./EsreverAnswers.md) ||

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Transposition Ciphers ](Contents.md) ||
| [ < Transposition ](./Transposition.md) | **Esrever** | [ Scytale > ](./Scytale.md) |
|| [ ! Esrever (Answers) ](./EsreverAnswers.md) ||
