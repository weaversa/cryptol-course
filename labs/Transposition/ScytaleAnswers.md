# Introduction

This module defines a classic transposition "cipher" that encrypts a 
message by wrapping it around a (virtual) [scytale](https://en.wikipedia.org/wiki/Scytale).  
The cipher is defined in terms of the 
`labs::Transposition::Transposition` library, which defines `encrypt` 
and `decrypt` functions given a permutation mapping, which in this 
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

For some background, read 
[_Programming Cryptol_](https://cryptol.net/files/ProgrammingCryptol.pdf)'s 
section on Scytale.

## Skills You'll Learn

This module will demonstrate module imports and sequence 
manipulation, and offer more practice specifying transposition ciphers.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the 
Cryptol interpreter. Load this module from within the Cryptol 
interpreter running in the `cryptol-course` directory with:

```shell
Cryptol> :m labs::Transposition::ScytaleAnswers
```

We start by defining the module for this lab:

```cryptol
module labs::Transposition::ScytaleAnswers where
```

Additionally, we will import the common transposition cipher 
definitions:

```cryptol
import labs::Transposition::TranspositionAnswers
```

# Scytale Encryption and Decryption

After reading the Scytale section of _Programming Cryptol_, recall 
its definitions of `scytale` and `dScytale` (refactored slightly):

```cryptol
scytale:
    {row, diameter}
    (fin row, fin diameter) =>
    String (row * diameter) -> String (diameter * row)
scytale msg = join (transpose msg')
  where
    msg' = split msg : [diameter][row]Char

dScytale:
    {row, diameter}
    (fin row, fin diameter) =>
    String (diameter * row) -> String (row * diameter)
dScytale msg = join (transpose msg')
  where
    msg' = split msg : [row][diameter]Char
```

While informative, this definition only accepts messages whose length 
is a multiple of the rod diameter:

```sh
labs::Transposition::ScytaleAnswers> scytale`{3} "ATTACKATDAWN"
"AAAATCTWTKDN"
labs::Transposition::ScytaleAnswers> scytale`{3} "ATTACKDAWN"

[error] at <interactive>:1:13--1:25:
  10 != 3 * anything
```

**EXERCISE**: Can we account for uneven message lengths and define a 
similar transposition for any message length and rod diameter?

Our strategy will be to rephrase scytale to operate over a 
permutation mapping with the number of padding characters necessary 
to reach a multiple of the rod diameter, then use `unpad` to reduce 
that mapping back down to the original message length.  To support 
this, Cryptol defines type operators `/^` and `%^` to denote the 
number of blocks and padding length, respective, given message and 
block size.  Revisiting the earlier example, `"ATTACKDAWN"` is length 
10, requiring 4 blocks of 2 padding characters to divide into blocks 
of size 3:

```sh
labs::Transposition::ScytaleAnswers> `numBlocks:Integer where type numBlocks = 10 /^ 3
4
labs::Transposition::ScytaleAnswers> `padLength:Integer where type padLength = 10 %^ 3 
2
```

For our permutation mapping, however, instead of working with 
specific text (e.g. "ATTACKDAWN"), we work with corresponding 
indices.

**EXERCISE**: Define `pi`, the reduced permutation mapping for a 
padded message of length `n` on a scytale of rod diameter `r`.
(The identity permutation is produced from a rod of diameter `1`.)  
Use `pi_test` to check your definition.

```cryptol
pi: {d, n, w} (fin d, d >= 1, fin n, Cmp w, Integral w, Literal n w) => [n]w
pi = inverse (unpad`{n} (join (transpose (groupBy`{d} (take`{np} [0...])))))
  where
    type np = n /^ d * d
```

```cryptol
property pi_test = 
    and [ encrypt pi`{3}              "ATTACKATDAWN" == "ACDTKATAWATN"
        , encrypt pi`{3}                "ATTACKDAWN" == "ACATKWTDNA"
        , encrypt pi`{3} "WEAREDISCOVEREDFLEEATONCE" == "WOEEVEAEARRTEEODDNIFCSLEC" ]
```

**EXERCISE**: Can you state an agreement property in terms of 
`scytale` and `encrypt pi` using `rearrange` or `partition`?  What 
corner cases must be ruled out?  Define such a property 
`pi_agrees` and use it to verify your definition of `pi` for various 
rod diameters and message lengths.

```cryptol
scy_pad:
    {d, n}
    (n >= n / d * (n %^ d)) =>
    (fin d, d >= 1, fin n) =>
    String n -> String (n /^ d * d)
scy_pad msg = (msg @@ ixs') # join [ (msg @@ g) # "-" | g <- gixs_]
  where
    (ixs', ixs_) = splitAt`{n - (n / d) * (n %^ d)} (take`{n} [0...])

    gixs_ = split ixs_ : [n %^ d][n / d]Integer
```

```cryptol
pi_agrees:
    {d, n}
    (n >= n / d * (n %^ d)) =>
    (fin d, d >= 1, fin n) => String n -> Bit
pi_agrees msg =
    ~ (elem '-' msg) ==>
    encrypt pi`{d} msg == msg'
  where
    msg' = take`{n} (rearrange' (scytale`{n /^ d, d} (scy_pad`{d} msg)))
```

(Note: `rearrange'` accommodates larger message sizes than 
`rearrange`, but proving this property blows up around `n = 16`.)

# Conclusion

This lab presented the Scytale transposition cipher, building upon 
Galois, Inc.'s previous work to define a more generic version that 
accommodates arbitrary message sizes and is specified in terms of 
reusable transposition cipher components.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

Up: [Course README](../../README.md)
Previous: [Esrever: A trivial message-reversing transposition "cipher"](ErseverAnswers.md)
Next: [Rail Fence: A basic transposition cipher for humans that's not-so-basic for Cryptol](RailFenceAnswers.md)
