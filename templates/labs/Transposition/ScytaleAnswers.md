# Introduction

This module defines a classic transposition "cipher" that encrypts a 
message by wrapping it around a (virtual) [scytale](https://en.wikipedia.org/wiki/Scytale).  
The cipher is defined in terms of the 
`labs::Transposition::Transposition` library, which defines `encrypt` 
and `decrypt` functions given a permutation mapping reflecting 
positions on a scytale.

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

```Xcryptol-session
Cryptol> :m labs::Transposition::ScytaleAnswers
Loading module Cryptol
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofsAnswers
Loading module labs::Transposition::CommonPropertiesAnswers
Loading module labs::Transposition::TranspositionAnswers
Loading module labs::Transposition::ScytaleAnswers
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
    {diameter, row}
    (fin diameter, fin row) =>
    String (row * diameter) -> String (diameter * row)
scytale msg = join (transpose msg')
  where
    msg' = split msg : [diameter][row]Char

dScytale:
    {diameter, row}
    (fin diameter, fin row) =>
    String (diameter * row) -> String (row * diameter)
dScytale msg = join (transpose msg')
  where
    msg' = split msg : [row][diameter]Char
```

While informative, this definition only accepts messages whose length 
is a multiple of the rod diameter:

```Xcryptol-session
labs::Transposition::ScytaleAnswers> :s ascii=on
labs::Transposition::ScytaleAnswers> scytale`{3} "ATTACKATDAWN"
"ACDTKATAWATN"
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
10, requiring 4 blocks and 2 padding characters to divide into blocks 
of size 3:

```Xcryptol-session
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
Define `pi'` to be its `inverse`.  Use `pi_test` to check your 
definitions.

(Hint: consider `inverse` as a helper function.  Yes, for scytale 
_encryption_.  Bizarre, right?)

```cryptol
pi: {d, n} (fin d, d >= 1, fin n) => [n][width n]
pi = inverse (unpad`{n} (join (transpose (groupBy`{d} (take`{np} [0...])))))
  where
    type np = n /^ d * d

pi': {d, n} (fin d, d >= 1, fin n) => [n][width n]
pi' = inverse pi`{d}
```

```cryptol
property pi_test = and
    [ encrypt pi`{3}                          "" == ""
    , encrypt pi`{3}              "ATTACKATDAWN" == "ACDTKATAWATN"
    , encrypt pi`{3}                "ATTACKDAWN" == "ACATKWTDNA"
    , encrypt pi`{3} "WEAREDISCOVEREDFLEEATONCE" == "WOEEVEAEARRTEEODDNIFCSLEC" 
    , decrypt pi`{3}                          "" == ""
    , decrypt pi`{3}              "ACDTKATAWATN" == "ATTACKATDAWN"
    , decrypt pi`{3}                "ACATKWTDNA" == "ATTACKDAWN"
    , decrypt pi`{3} "WOEEVEAEARRTEEODDNIFCSLEC" == "WEAREDISCOVEREDFLEEATONCE" ]
```

```Xcryptol-session
labs::Transposition::ScytaleAnswers> :check pi_test
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

**EXERCISE**: Can you state an agreement property in terms of 
`scytale` and `encrypt pi` using `rearrange` or `partition`?  What 
corner cases must be ruled out?  Considering the physical properties 
of a scytale (as represented by the Wikipedia example), would `unpad` 
help here?  If not, how can you account for padding?  Define 
`pi_correct` and use it to verify your definition of `pi` for various 
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
    gixs_ = split ixs_ : [n %^ d][n / d][width (n /^ d * d)]
```

```cryptol
pi_correct:
    {d, n}
    (n >= n / d * (n %^ d)) =>
    (fin d, d >= 1, fin n) => String n -> Bit
pi_correct msg =
    ~ (elem '-' msg) ==>
    encrypt pi`{d} msg == msg'
  where
    msg' = take`{n} (rearrange' (scytale`{d} (scy_pad`{d} msg)))
```

(Note: `rearrange'` accommodates larger message sizes than 
`rearrange`, but proving this property blows up around `n = 16`.)

```Xcryptol-session
labs::Transposition::ScytaleAnswers> :prove pi_correct`{3, 10}
Q.E.D.
(Total Elapsed Time: 0.141s, using Z3)
labs::Transposition::ScytaleAnswers> :prove pi_correct`{3, 12}
Q.E.D.
(Total Elapsed Time: 0.251s, using Z3)
labs::Transposition::ScytaleAnswers> :check pi_correct`{3, 25}
Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^200 values)
```

# Conclusion

This lab presented the Scytale transposition cipher, building upon 
Galois, Inc.'s previous work to define a more generic version that 
accommodates arbitrary message sizes and is specified in terms of 
reusable transposition cipher components.

{{ solicitation }}

{{ navigation }}
