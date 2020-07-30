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

```shell
Cryptol> :m labs::Transposition::EsreverAnswers
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
`msg`.  Check it with `pi_test` and prove it using  `pi_reverses`.

```cryptol
/** a permutation mapping that just reverses order */
pi: {n, w} (fin n, Integral w, Literal 0 w) => [n]w
pi = reverse (take`{n} [0...])
```

```cryptol
property pi_test = and
    [ encrypt pi "" == ""
    , encrypt pi "A" == "A"
    , encrypt pi "STRESSED" == "DESSERTS"
    , encrypt pi "RACECAR" == "RACECAR"
    , encrypt pi "SEMORDNILAP" == "PALINDROMES"
    ]

/** `encrypt pi` reverses sequence order */ 
pi_reverses: {n, a} (fin n, Eq a) => [n]a -> Bit
pi_reverses msg = (encrypt pi) msg == reverse msg
```

# Conclusion

This lab presented abstract definitions for transposition ciphers, 
formalizing definitions for permutations and inverses.  Subsequent 
labs will provide specific examples of transposition ciphers.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

Up: [Course README](../../README.md)
Previous: [Transportation Ciphers, in the Abstract](TranspositionAnswers.md)
Next: [Scytale: A classic easy-to-specify transposition cipher](ScytaleAnswers.md)
