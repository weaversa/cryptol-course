# Introduction

Key Wrapping is an important technique for storing and transmitting
cryptographic keys. This module introduces a family of NIST's general
purpose Key Wrapping algorithms.

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in this file.
  
You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * Cryptol's sequence and `Integer` types,
  * demoting types variables to value variables,
  * the `:prove` command,
  * manipulating sequences using `#`, `take`, `drop`, `split`, `join`,
    `head`, and `tail`,
  * writing functions and properties,
  * sequence comprehensions,
  * functions with curried parameters,
  * logical, comparison, arithmetic, and conditional operators.

## Skills You Will Learn

By the end of this lab you will have read through a NIST standard and
implemented a few real-world block cipher modes for authenticated
encryption and decryption.

You'll also gain experience with
  * type parameters and type constraints,
  * pattern matching,
  * the use of pre-written cryptographic routines from other modules,
  * navigating some nuances of Cryptol's type checking system, and
  * the `foldl` operator.

## Load This Module

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming)
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::KeyWrapping::KeyWrappingAnswers
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::GF28
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::State
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::SubBytePlain
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::SBox
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::SubByteSBox
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::Round
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::Algorithm
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::ExpandKey
Loading module specs::Primitive::Symmetric::Cipher::Block::AES::TBox
Loading module specs::Primitive::Symmetric::Cipher::Block::AES_parameterized
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module specs::Primitive::Symmetric::Cipher::Block::TripleDES
Loading module labs::KeyWrapping::KeyWrappingAnswers
```

We start by defining a new module for this lab:

```cryptol
module labs::KeyWrapping::KeyWrappingAnswers where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.  
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Writing Key Wrapping Routines in Cryptol

This lab takes the student through developing wrapping algorithms
described in [NIST Special Publication
800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final)
"Recommendation for Block Cipher Modes of Operation: Methods for Key
Wrapping". We recommend you have this lab and the specification
document open side-by-side.

Here is the abstract from this document:

> This publication describes cryptographic methods that are approved
> for "key wrapping," i.e., the protection of the confidentiality and
> integrity of cryptographic keys. In addition to describing existing
> methods, this publication specifies two new, deterministic
> authenticated-encryption modes of operation of the Advanced
> Encryption Standard (AES) algorithm: the AES Key Wrap (KW) mode and
> the AES Key Wrap With Padding (KWP) mode. An analogous mode with the
> Triple Data Encryption Algorithm (TDEA) as the underlying block
> cipher, called TKW, is also specified, to support legacy
> applications.

In this lab we will focus on developing the three main algorithms --
`KW`, `TKW`, and `KWP` -- by building up the necessary
subcomponents. In fact each of these is a family of algorithms: `KW`
is composed of an *authenticated encryption* component `KW-AE` and an
*authenticated decryption* component `KW-AD`; similarly for `TKW` and
`KWP`.

# Preliminaries

The [NIST Special Publications 800
Series](https://www.nist.gov/itl/publications-0/nist-special-publication-800-series-general-information)
provides information of interest to the computer security
community. The series comprises guidelines, recommendations, technical
specifications, and annual reports of NIST's cybersecurity activities.

Reading through and implementing a formal specification in Cryptol for
one of the cryptography standards in this series can be a
challenge. The standards are written by a variety of authors and the
algorithms are often described in one-off language-agnostic pseudo
code which we have to parse. This translation process can lead to
subtle implementation errors or potentially false assumptions about
the algorithms which can be hard to spot.

Our job is to extract the relevant details from the specification and
build Cryptol specifications for the three main algorithms listed
above.


# Getting Started

The Key Wrapping algorithms described in this document are a form of
[Block Cipher
Mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation)
for an existing block cipher. Section 5.1 (p. 8) of the standard
indicates:

> For KW and KWP, the underlying block cipher shall be approved, and
> the block size shall be 128 bits. Currently, the AES block cipher,
> with key lengths of 128, 192, or 256 bits, is the only block cipher
> that fits this profile. For TKW, the underlying block cipher is
> specified to be TDEA, and the block size is therefore 64 bits; the
> KEK for TKW may have any length for which TDEA is approved; see [8].

That is, `KW` (and `KWP`) are defined to operate with
[`AES`](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)
for its three key sizes: 128, 192, and 256 bits.

The standard does not indicate a specific key size for
[`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), but
`TDEA`/Triple-DES is typically used with a 192-bit key and that is
what we will develop later in this lab.

Also, we will not develop `AES` or `TDEA` in this lab. Instead we will
use pre-written modules which provide these block cipher
primitives. The algorithms are found under the `specs/` directory in
`specs/Primitive/Symmetric/Cipher/Block` and we import them into our
module with the following:

```cryptol
import specs::Primitive::Symmetric::Cipher::Block::AES_parameterized as AES
import specs::Primitive::Symmetric::Cipher::Block::TripleDES as TDEA
```

Cryptol modules must match the directory structure they reside
in. Using descriptive names as we do here is a good way to organize
related algorithms by type, function, or whatever works for your
system.

Now is a good time to scan through the specification document and get
a sense of the overall organization:

 * **Sections 1 - 3, Purpose, Authority, and Introduction** -- These
     sections provide background, usage, and cryptographic function of
     the algorithms described in this document. This information is
     good background if we were trying to decide *how* to use these
     algorithms; however we will not need to reference this
     information to build our specifications.

 Feel free to skim through this material or skip for now.

 * **Section 4, Definitions and Notation** -- This section contains
     important definitions, acronyms, variables, and operations used
     in this standard. Let's scan through this to see if we find
     anything useful...

Section 4.3 provides some constants `ICV1`, `ICV2`, and `ICV3` which
are defined to have special values.

```cryptol
ICV1 = 0xA6A6A6A6A6A6A6A6
ICV2 = 0xA65959A6
ICV3 = 0xA6A6A6A6
```

Section 4.4 introduces operators and notation for cryptographic
functions and their building blocks. We have already imported the
required block ciphers and we will be building some of these for
ourselves. For the remainder, Cryptol provides analogous functionality
to us in some fashion or another.

**EXERCISE**: Fill in definitions for the operators given in Section
4.4. Use the properties defined in Section 4.5 (also given below) to
show that your answers are correct.

```cryptol
//0^^s -- The bit string that consists of s consecutive '0' bits.
//0 : [s]

/**
 * The integer for which the bit string X is the binary
 * representation.
 */ 
int X = toInteger X

/**
 * The bit length of bit string X.
 */
len X = length X

/**
 * The bit string consisting of the s right-most bits
 * of the bit string X.
 */
LSB : {s, a} (fin s, fin a, a >= s) => [a] -> [s]
LSB X = drop X

/**
 * The bit string consisting of the s left-most bits of
 * the bit string X.
 */
MSB : {s, a} (fin s, a >= s) => [a] -> [s]
MSB X = take X

// [x]s -- The binary representation of the non-negative integer
//         x as a string as a string of s bits, where x < 2^^s.
//`fromInteger` transforms an Integer into a bitvector.

// The bitwise exclusive-OR of bit strings X and Y whose bit
// lengths are equal.
// X ^ Y

// The concatenation of bit strings X and Y
// X # Y
```

Section 4.5 contains properties of the operators given in Section
4.4.

```cryptol
property hexadecimalProp = 0xA659 == 0b1010011001011001

property zeroBitsProp = (0 : [8]) == 0b00000000

property concatenationProp = 0b001 # 0b10111 == 0b00110111

property XORProp = 0b10011 ^ 0b10101 == 0b00110

property lenProp = len 0b00010 == 5

property LSBProp = LSB`{3} 0b111011010 == 0b010

property MSBProp = MSB`{4} 0b111011010 == 0b1110

property bitstringProp = fromInteger 39 == 0b00100111

property intProp = int 0b00011010 == 26
```

 * **Section 5, Preliminaries** -- This section covers usage and
     information about data size restrictions. The most pertinent
     parts are those defining what a semiblock is (which, as it turns
     out, is simply a 64-bit word when used in conjunction with AES,
     and a 32-bit word when used in conjunction with TDEA) and Table 1
     that provides limits on the size of the input and outputs.

 * **Section 6, Specifications of KW and KWP** -- This section
     specifies the two families of algorithms `KW` and `KWP`. This
     section is the reference we will use for the bulk of this lab as
     we work through building a specification for `KW`.

 * **Section 7, Specification of TKW** -- This section specifies the
     final major algorithm `TKW`. It is structurally very similar to
     `KW`, but there are some differences that warrant its own section
     in the standards.

 * **Section 8, Conformance** -- This section has information about
     how implementations may claim conformance to the algorithms
     described in this standard.

# Formal Specification of `KW`

`KW` is a family of algorithms comprised of `KW-AE` and `KW-AD`. We
start with `KW-AE`.

## Building a Formal Specification for `KW-AE`

Let's take a look at writing a specification for the `KW-AE` which is
presented in **Section 6**. We'll call our function `KWAE` in Cryptol
because we cannot use the dash/minus sign when naming functions.

The document indicates that `KW-AE` depends on a *wrapping function*
`W` (Algorithm 1, page 11). This algorithm has certain *prerequisites*
which we will have to model in our formal specification:

  * A **Key Encryption Key (KEK)** `K` and
  * A **designated cipher function** `CIPHk`, which operates on 128-bit blocks

The document defines a *semiblock* to be a block with half the width
of the underlying block cipher, `CIPHk`. Since `KW-AE` uses `AES` as
its `CIPHk`, semiblocks will be 64-bit blocks. Also notice that the
specification for `W` defines the *Input* to be a string `S` of `n`
semiblocks and the *Output* will be a transformed string `C` of `n`
semiblocks. *`W` was previously defined in Section 4.4 to take a
bitstring, not a sequence of semiblocks*, so we will write `W` to
consume bitstrings, but we will have to split `W` into semiblocks
internally. We now have enough to build a simple type signature for
`W` which will contain the following components:

 * `n` -- A *type parameter* which controls the number of semiblocks
   in our inputs and outputs
 * `([128] -> [128])` -- The type of our *keyed* block cipher `CIPHk`
 * `[n * 64]` -- The type of our string of input semiblocks
 * `[n * 64]` -- The type of our transformed output

Putting these together we have our preliminary type signature:

```comment
W_prelim :
  {n}
  (fin n) =>
  ([128] -> [128]) -> [n * 64] -> [n * 64]
```

We haven't quite captured enough about the type of `W` -- for the
algorithm to operate correctly, and according to the standard, we will
have to add two more constraints on `n`, namely, that `n >= 3` and
`fin n`.

```comment
W :
  {n}
  (fin n, 3 <= n) =>
  ([128] -> [128]) -> [n * 64] -> [n * 64]
```

Taking a close look at `Algorithm 1` we can see that `W`'s step 2
transforms its inputs over a series of `s` rounds. It will make our
job easier to model this step of the function first. Also, it may help
to understand this step by studying `Figure 2` on page 12.

`WStep2` will inherit the same type parameters, inputs, and outputs as
`W`. We add a new input `t` of type `Integer` which serves as the
round counter found in step 2 of `Algorithm 1`. We also use pattern
matching to pull out specific entries in the sequence of semiblocks,
i.e. `([A, R2] # Rs)` gives names to the first two semiblocks of the
input.

**EXERCISE**: Study `Algorithm 1` and `Figure 2` and implement
`WStep2`.

```cryptol
WStep2:
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> Integer -> [n][64]
WStep2 CIPHk ([A, R2] # Rs) t = [A'] # Rs # [Rn]
  where
    A'  = MSB (CIPHk (A # R2)) ^ (fromInteger t)
    Rn  = LSB (CIPHk (A # R2))
```

*Note:* Pattern matching (a kind of shorthand) is used for one of the
parameters to `WStep2`. According to the type signature, the second
parameter is of type `[n][64]` -- a sequence of semiblocks. In the
definition of `WStep2` we see that this parameter is identified as
`([A, R2] # Rs)`. Cryptol assigns the *first* semiblock to `A`, the
*second* semiblock to `R2`, and all the remaining semiblocks to
`Rs`. Since we have the type parameter condition `n >= 3` We know that
there are at least three such blocks to assign, and at least one will
be assigned to `Rs`.

*Observation*: It's possible that requiring `n >= 3` is a mistake. We
say this because (and you can test this) Cryptol also accepts the
definitions of `W` and `WStep2` with type constraint `n >= 2` instead
of `n >= 3`. In the case where `n == 2`, `Rs` is simply the empty
sequence. Unfortunately, this and the use of 1-based indexing
(starting `t` at `1`) causes reverberations throughout the rest of the
specification, culminating in a more-complex-than-necessary `KWP`
function.

Given `WStep2`, it is a simple matter to complete the definition for
`W` that we started above. But first we take a quick aside to recall
the `foldl` operator which will come in very handy.

*But first...*

### A Quick Aside on `foldl`

`foldl` is a Cryptol Primitive and [higher order
function](https://en.wikipedia.org/wiki/Higher-order_function) which
is useful for (among other things) extracting the final state from
some iterative process.

The signature for `foldl` is as follows:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :t foldl
foldl : {n, a, b} (fin n) => (a -> b -> a) -> a -> [n]b -> a
```

We see that `foldl` takes as inputs the following data

 * A function of type `(a -> b -> a)` which transforms a "state" of
   type `a` into a new one by processing an element of type `b`
 * An element of type `a` (an initial state value) and
 * A sequence of elements of type `b`

One application for `foldl` is to access the final element of some
iterative process. For instance, we **could** find a list of partial
sums from the sequence `[1..10]` as follows:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> sums where sums = [0] # [ x + partial | x <- [1..10] | partial <- sums]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
[0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
```

However, if we are only interested in the final element of this
sequence, then we can use `foldl` as follows:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> foldl (+) 0 [1..10]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
55
```
*...we now return to our regularly scheduled program.*

We will use `foldl` along with our step function `WStep2` to write a
definition for `W`.

**EXERCISE**: Complete the definition of `W` below by filling in the
  function skeleton provided.

```cryptol
W :
    {n}
    (fin n, 3 <= n) =>
    ([128] -> [128]) -> [n * 64] -> [n * 64]
W CIPHk S = join C
  where
    type s = 6 * (n-1)
    S'     = split S
    C      = foldl (WStep2 CIPHk) S' [1..s]
```

With `W` in hand there it isn't much more work to complete the `KW-AE`
algorithm.

**EXERCISE**: Study `Algorithm 3` and complete the specification for
  `KW-AE` by filling in the snippet below with the correct definition
  for `S` and `C`. Remember that `ICV1` was defined earlier in this
  module. Also, notice that the bounds from Table 1 (Section 5.3.1,
  page 10) are included as type constraints.

```cryptol
KWAE :
    {n}
    (fin n, 2 <= n, n < 2^^54) =>
    ([128] -> [128]) -> [n * 64] -> [(n+1) * 64]
KWAE CIPHk P = C
  where
    S = ICV1 # P
    C = W CIPHk S
```

At this point you can check your work against six test vectors given
in a property defined later on in this document. Here is the the
command and sample output for `KWAETests`.

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :check KWAETests
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Building a Formal Specification for `KW-AD`

It should be fairly straightforward at this point to implement the
authenticated decryption function `KW-AD` which is the other algorithm
in the `KW` family. There is one significant difference, though: since
the algorithm *authenticates* the decryption we will need to add an
authentication check and slightly alter the type for `KW-AD`.

**EXERCISE**: Review `Algorithm 2` and `Figure 3` of the document and
complete the definitions for the inverse routines to `W` and `WStep2`
which we shall call `W'` and `WStep2'` respectively. The type
declarations for these functions are provided.

*Hint:* Notice that, except for the names, the type declarations are
identical. The function definitions are also very similar. Pay
special attention to the order of the index variable for the main
loop, the sequence of operations, and how the sequence of `Rs`
transforms:

```cryptol
WStep2' :
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> Integer -> [n][64]
WStep2' CIPHk' ([A] # Rs # [Rn]) t = [A', R2] # Rs
  where
    A'  = MSB (CIPHk' ((A ^ (fromInteger t)) # Rn))
    R2  = LSB (CIPHk' ((A ^ (fromInteger t)) # Rn))

W' :
    {n}
    (fin n, 3 <= n) =>
    ([128] -> [128]) -> [n * 64] -> [n * 64]
W' CIPHk' C = join S
  where
    type s = 6 * (n-1)
    C'     = split C
    S      = foldl (WStep2' CIPHk') C' [ s, s-1 .. 1 ]
```

Once you have these completed you should be able to check your work by
having Cryptol `:prove` the properties `WStep2'Prop` and
`W'Prop`. Your output should look something like the following:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :prove WStep2'Prop
Q.E.D.
(Total Elapsed Time: 0.064s, using "Z3")
labs::KeyWrapping::KeyWrappingAnswers> :prove W'Prop
Q.E.D.
(Total Elapsed Time: 0.618s, using "Z3")
```

These two properties state that for a fixed, dummy CIPHk and S of
length 3 semiblocks, `WStep2` and `WStep2'` are inverses and `W` and
`W'` are inverses. Here are the definitions of these properties:

```cryptol
property WStep2'Prop ARs t =
    WStep2'`{3} (\a -> a-1) (WStep2 (\a -> a+1) ARs t) t == ARs

property W'Prop S =
    W'`{3} (\a -> a-1) (W (\a -> a+1) S) == S
```

The final step is to use these components to write the authenticated
decryption algorithm `KW-AD`. Unlike `W'` and `WStep2'` this function
will have a different type than its related routine in the `KW-AE`
family because it needs to capture whether or not the ciphertext
authenticates *as well as* computes the corresponding plaintext.

**EXERCISE**: Study `Algorithm 4` from the standard and complete the
  definition of `KWAD` below by filling in the function skeleton
  provided. This function needs both an appropriate definition for
  `S`, a computation of the authentication bit to assign to `FAIL`,
  and finally the appropriate plaintext.

Notice that `FAIL` indicates a *failure* to authenticate so should be
`False` for authenticated decryptions and `True` for failures to
authenticate.

```cryptol
KWAD :
    {n}
    (fin n, 2 <= n, n < 2^^54) =>
    ([128] -> [128]) -> [(n+1) * 64] -> (Bit, [n * 64])
KWAD CIPHk' C = (FAIL, P)
  where
    S    = W' CIPHk' C
    FAIL = MSB S != ICV1
    P    = LSB S
```

When you have successfully defined this function, you can test your
work by `:prove`ing that `KWAE` and `KWAD` are inverses (well, at
least for a dummy CIPHk and P of length 3 semiblocks) using the
`KWAEInvProp`. You can also `:check` your work against six test
vectors by using the property `KWADTests` (this is defined later on in
this document).

```cryptol
property KWAEInvProp S =
    KWAD`{3} (\a -> a-1) (KWAE (\a -> a+1) S) == (False, S)
```

# Formal Specifications of `TKW-AE` and `TKW-AD`

We're briefly skipping over the definitions of `KWP-AE` and `KWP-AD`
in Section 6.3 because the definitions of `TWK-AE` and `TWK-AD` in
Section 7 are very similar to `KW-AE` and `KW-AD`. We'll come back to
`KWP` a little later.

**EXERCISE**: Try your hand at writing the specification for `TKW-AE`
  and `TKW-AD` from Section 7. These functions are very similar to
  `KW-AE` and `KW-AD` but they use the 64-bit block cipher `TDEA`
  (also known as
  [Triple-DES](https://en.wikipedia.org/wiki/Triple_DES)). We
  recommend following the same steps from above and defining the helper
  functions `TWStep2`, `TWStep2'`, `TW`, and `TW'` before you attempt
  `TKW-AE` and `TKW-AD`. Test vectors are available in the
  [kwtestvectors directory](./kwtestvectors/).

There are some minor modifications to be made, but things should go
easily using `KW-AE` and `KW-AD` as a reference. This is a good
opportunity to go back through and take a close look at the type
parameters and conditions and be sure you understand what they mean
and how to use them. One important thing to note is that had we
defined our functions above using a `semigroup` type parameter (rather
than hard-code `128` and `64`), our work defining the TKW family of
functions would already be done! Consider working through the [Simon
and Speck lab](../SimonSpeck/SimonSpeck.md) next. There you'll learn
how to write parameterized modules -- imagine writing a Cryptol module
that takes in the semiblock size as a parameter and defines `W` and
`W'`, and then importing that module with `semiblock = 64` into an AES
key wrap module, and with `semiblock = 32` into a TDES key wrap
module. This concept of parameterized, hierarchical modules can really
help you make clear, duplication free, reusable Cryptol
specifications. For those with poor imaginations, we've provided such
a thing [here](./spec/).

**Back to TDES**: One important difference in the TKW family is you
will use the Triple-DES algorithm that's implemented in the `TDEA`
module we imported earlier. You can check the type of `TDEA` in the
interpreter via `:t TDEA::blockEncrypt` and `:t
TDEA::blockDecrypt`. This has a slightly different interface than the
block cipher we used from the `AES` module earlier. You can view how
we use it here by looking at `TestTKWAE` and `TestTKWAD`. It is worth
taking a quick look through the `TripleDES.cry` to learn a little bit
about a particularly famous NIST test vector.

You can test your work with the `TKWAETests` and `TKWADTests`
properties. Good luck!

```cryptol
TWStep2:
    {n}
    (fin n, n >= 3) =>
    ([64] -> [64]) -> [n][32] -> Integer -> [n][32]
TWStep2 CIPHk ([A, R2] # Rs) t = [A'] # Rs # [Rn]
  where
    A'  = MSB (CIPHk (A # R2)) ^ (fromInteger t)
    Rn  = LSB (CIPHk (A # R2))

TW :
    {n}
    (fin n, 3 <= n) =>
    ([64] -> [64]) -> [n * 32] -> [n * 32]
TW CIPHk S = join C
  where
    type s = 6 * (n-1)
    S'     = split S
    C      = foldl (TWStep2 CIPHk) S' [1..s]

TKWAE :
    {n}
    (fin n, 2 <= n, n < 2^^28) =>
    ([64] -> [64]) -> [n * 32] -> [(n+1) * 32]
TKWAE CIPHk P = C
  where
    S = ICV3 # P
    C = TW CIPHk S

TWStep2' :
    {n}
    (fin n, n >= 3) =>
    ([64] -> [64]) -> [n][32] -> Integer -> [n][32]
TWStep2' CIPHk' ([A] # Rs # [Rn]) t = [A', R2] # Rs
  where
    A'  = MSB (CIPHk' ((A ^ (fromInteger t)) # Rn))
    R2  = LSB (CIPHk' ((A ^ (fromInteger t)) # Rn))

TW' :
    {n}
    (fin n, 3 <= n) =>
    ([64] -> [64]) -> [n * 32] -> [n * 32]
TW' CIPHk' C = join S
  where
    type s = 6 * (n-1)
    C'     = split C
    S      = foldl (TWStep2' CIPHk') C' [ s, s-1 .. 1 ]

TKWAD :
    {n}
    (fin n, 2 <= n, n < 2^^28) =>
    ([64] -> [64]) -> [(n+1) * 32] -> (Bit, [n * 32])
TKWAD CIPHk' C = (FAIL, P)
  where
    S    = TW' CIPHk' C
    FAIL = MSB S != ICV3
    P    = LSB S
```


# A Formal Specification of `KWP-AE`

`KWP-AE` is the authenticated-encryption function and makes use of our
previously defined `W`. There is a new concept to introduce with this
specification.

## Oddly Typed `if-then-else` Statements

Sometimes, though not often, cryptographic algorithms will contain
`if` statements where the `then` and `else` branches return different
types. You were exposed to this a bit already in the [Salsa20
lab](../Salsa20/Salsa20.md). First off, this is always frustrating to
deal with in Cryptol, and we want you to know that we feel your pain
and we're sorry. Cryptol _can_ handle these types of situations, but
coming up with a solution requires experience with the type system
that is likely only learned through trial and error.

To dig into this a bit, let's consider the type of a generic
`if-then-else` statement

```Xcryptol-session
labs::KeyWrapping::KeyWrapping> :t \(c, t, e) -> if c then t else e
(\(c, t, e) -> if c then t else e) : {a} (Bit, a, a) -> a
```

Here we see that the condition `c` has to be of type `Bit`. This makes
perfect sense given that `c` is a condition. Next, notice that the
types of `t` and `e`, the values returned by the `then` and `else`
cases, both have to have the same type. Here we see that Cryptol
doesn't support `if-then-else` statements that return different
types. So, when we see a specification that purports to do such a
thing, we can either give up, or try and unify the two cases.

Here is a silly example that closely models the behavior we'll see in
`KWP-AE` and `KWP-AD`:

```cryptol
g : [32] -> [32]
g x = x + 1

h : [64] -> [64]
h x = x - 1
```

```comment
f : {a} (fin a, 32 <= a, a <= 64) => [a] -> [48]
f x = if `a <= 0x30 then
          g x
        else
          h x
```

Here we have a function `f` that takes an `a`-bit bitvector as input
where `a` is some length between `32` and `64` bits. `f` always
returns `48` bits. Here's the strange part --- if `a <= 0x30` (`0x30`
is `48`), `f` returns `g x`, where `g` takes and returns only 32-bit
values. If `a > 0x30`, `f` returns `h x` where `h` takes and returns
only 64-bit values. If we try to load this function into Cryptol we
see:

```Xcryptol-session
[error] at labs/KeyWrapping/KeyWrapping.md:863:1--866:14:
  Failed to validate user-specified signature.
    in the definition of 'f', at labs/KeyWrapping/KeyWrapping.md:863:1--863:2,
    we need to show that
      for any type a
      assuming
        • fin a
        • 32 <= a
        • a <= 64
      the following constraints hold:
        • a == 64
            arising from
            matching types
            at labs/KeyWrapping/KeyWrapping.md:866:13--866:14
        • a == 32
            arising from
            matching types
            at labs/KeyWrapping/KeyWrapping.md:864:13--864:14
[error] at labs/KeyWrapping/KeyWrapping.md:864:11--864:12:
  Type mismatch:
    Expected type: 48
    Inferred type: 32
[error] at labs/KeyWrapping/KeyWrapping.md:866:11--866:12:
  Type mismatch:
    Expected type: 48
    Inferred type: 64
```

This message tells us that `a`, the length of our input, has to
simultaneously be both `64` and `32` and (looking at the line numbers)
that these constraints come from the types of `g` and `h`.

In support of fixing the function, notice that since `g` always
takes and returns 32-bit values, we have to shrink `x` from `a` bits
to `32` bits, and widen the result up to `48` bits. And, since `h`
always takes and returns 64-bit values, we have to widen `x` from `a`
bits to `64` bits, and shrink the result back down to `48` bits. To
help us do this resizing work, we'll introduce `shrink` and `widen`
functions.

```cryptol
widen : {a, b} (fin a, fin b) => [b] -> [a + b]
widen a = 0 # a

shrink : {a, b} (fin a, fin b) => [a + b] -> [b]
shrink a = drop a
```

`widen` takes any sized input and prepends `0` or more `False`
bits. `shrink` takes any sized input and removes `0` or more bits from
the front. Using these two functions, we can fix our `f` from above:

```cryptol
f : {a} (32 <= a, a <= 64) => [a] -> [48]
f x = if `a <= 0x30 then
        widen (g (shrink x))
      else
        shrink (h (widen x))
```

And here we test that `f` correctly calls `g` and `h` (which increment
and decrement by 1, respectively).

```Xcryptol-session
labs::KeyWrapping::KeyWrapping> f (10 : [37])
0x00000000000b
labs::KeyWrapping::KeyWrapping> f (10 : [53])
0x000000000009
```


## KWP-AE Top Level Function

With that consideration firmly under our belt, we can now tackle
`KWP-AE`.

**EXERCISE**: Study `Algorithm 5` from the standard and complete the
  definition of `KWPAE` below by filling in the function skeleton
  provided with appropriate logic. Use the `shrink` and `widen`
  functions to assist in resizing `S` and the outputs of `W` and
  `CIPHk` on the `then` and `else` branches of line 5.

*Hint:* You'll notice that we needed to pull in the type variable `n`
and the type constraints from `W`, as well as to relate `n` and `k`
(the types of `S` and `C`). You'll need to pass `n` into `W`, ensuring
that we avoid the `n == 2` case. This can be done by calling `W` like
so ``W`{max 3 n}``. Also, remember that `ICV2` was defined above, so
we do not need to redefine it inside `KWPAE`.

```cryptol
KWPAE :
    {k, n}              // k is [len(P)/8], Algorithm 5
    ( 1 <= k, k < 2^^32 // Bounds on the number of octets of P, from Table 1
    , 2 <= n, n == 1 + k /^ 8) => // Here we relate n and k
    ([128] -> [128]) -> [k * 8] -> [n * 64]
KWPAE CIPHk P = C
  where
    type padlen = 8 * (k*8 /^ 64) - k
    PAD = 0 : [8 * padlen]
    S = ICV2 # (fromInteger (len P / 8) : [32]) # P # PAD : [n * 64]
    C = if len P <= 64 then
          widen (CIPHk (shrink S))
        else
          shrink (W`{max 3 n} CIPHk (widen S))
```

Feel free to use the provided `KWPAETests` property to check your
work.

Here we point out our observation from earlier -- if the type
constraint on `n` in `W` had been `n >= 2` and had `t` started at `0`
rather than `1`, then `W CIPHk S == CIPHK S` and we wouldn't have
needed to test on the length of `P`. The definition of `C` would then
have been `C = W'{n} CIPHk S`. So, hopefully you see how an arbitrary
(mistaken?) constraint (compounded by 1-based indexing) percolated
through this specification and caused trouble.


# A Formal Specification of `KWP-AD`

`KWP-AD` is the authenticated-decryption function and makes use of our
previously defined `W'`.

**EXERCISE**: Study `Algorithm 6` from the standard and complete the
  definition of `KWPAD` below by filling in the function skeleton
  provided with appropriate logic. You'll notice we've used types and
  pattern matching to separate out the 4 components of S, rather than
  ask you to muck about defining `Plen`, `padlen`, `LSB...`, etc. In
  truth, it's not possible to follow the spec verbatim here because
  `Plen` is derived from a value variable (`S`) but later used to
  derive a type variable (`padlen` on line 6 which is then used as the
  size of `0` on line 8), and promotion of value variables to a type
  variables is explicitly forbidden in Cryptol.

```cryptol
KWPAD :
    {k, n}              // k is [len(P)/8], Algorithm 5
    ( 1 <= k, k < 2^^32 // Bounds on the number of octets of P, from Table 1
    , 2 <= n, n == 1 + k /^ 8) => // Here we relate n and k
    ([128] -> [128]) -> [n * 64] -> (Bit, [k * 8])
KWPAD CIPHk' C = (FAIL, P)
  where
    S = if `n == 2 then
          widen (CIPHk' (shrink C))
        else
          shrink (W'`{max 3 n} CIPHk' (widen C))
    Plen  : [32]
    PAD   : [k*8 %^ 64]
    ICV2' # Plen # P # PAD = S
    FAIL = ICV2' != ICV2                             \/
           Plen  != (fromInteger (len P / 8) : [32]) \/
           PAD   != 0
```

Feel free to use the provided `KWPADTests` property to check your
work.


## But what about the ciphertext limits?

I'm sure the sticklers in the class noticed that we reused the same
type constraints from `KWP-AE` which don't overtly mention the
ciphertext bounds from Table 1, namely, that the length of `C` must be
between `2` and `2^^29`, inclusively. Fortunately, those bounds
_should_ be inferred by the plaintext bounds. And, now that you
mention it, we can check! Table 1 tells us that, for `KWP`, if the
number of octets of `P` is the upper limit of `2^^32-1`, that the
number of semiblocks of `C` should be `2^^29`.

Asking Cryptol for the type of `KWPAE` after plugging in `2^^32-1` for
`k` gives an `l` of `34359738432`:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :t KWPAE`{k = 2^^32 - 1}
KWPAE`{k = 2 ^^ 32 -
           1} : ([128] -> [128]) -> [34359738360] -> [34359738432]
```

Well, what's `34359738432`? Is it `2^^29` 64-bit words? Let's first
check how many 64-bit words it is. Here's one way:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :t \(a : [34359738432]) -> groupBy`{64} a
(\(a : [34359738432]) -> groupBy`{64} a) : [34359738432] -> [536870913][64]
```

Great...now what's `536870913`? Is it `2^^29`?

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> 2^^29 : Integer
536870912
```

Woh! Its not. `536870913` is `2^^29 + 1`. Let's double check this ---
here is a command that tests the `2^^29` upper bound from Table 1:

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :t KWPAE`{k = 2^^32 - 1, n = (2^^29)}

[error] at <interactive>:1:1--1:6:
  Unsolvable constraints:
    • 536870912 == 536870913
        arising from
        use of expression KWPAE
        at <interactive>:1:1--1:6
    • Reason: It is not the case that 536870912 == 536870913
```

And here is a command that tests the bound we just found, `2^^29 + 1`.

```Xcryptol-session
labs::KeyWrapping::KeyWrappingAnswers> :t KWPAE`{k = 2^^32 - 1, n = (2^^29 + 1)}
KWPAE`{k = 2 ^^ 32 - 1,
       n = (2 ^^ 29 +
            1)} : ([128] -> [128]) -> [34359738360] -> [34359738432]
```

Well folks, it appears we (...well, Cryptol) just found a bug (albeit
a small one) in one of NIST's most well read crypto specs. Thanks
sticklers!

# Test Vectors

Test vectors were not included in NIST-SP-800-38F; however, the
`KW-AE`, `KW-AD` family of key-wrapping algorithm enjoy common usage
and are described and referenced in other standards material. The test
vectors in this section were drawn from [RFC
3394](https://tools.ietf.org/html/rfc3394).

Recall that you can check individual properties with the `:check`
command in the interpreter.  Here are some test vectors from [RFC
3394](rfc3394.pdf) that are useful for testing `KW-AE` and `KW-AD`.

```cryptol
TestKWAE :
   {a, n}
   (a >= 2, 4 >= a, n >= 2, 2^^54-1 >= n) =>
   [a*64] -> [n*64] -> [(n+1)*64]
TestKWAE k pt = ct
  where
    ct = KWAE (\p -> AES::encrypt k p) pt

property KWAETests =
    (TestKWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F ])
              (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ]) ==
     join [ 0x1fa68b0a8112b447, 0xaef34bd8fb5a7b82, 0x9d3e862371d2cfe5 ]) /\
    (TestKWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
              (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ]) ==
     join [ 0x96778b25ae6ca435, 0xf92b5b97c050aed2, 0x468ab8a17ad84e5d ]) /\
    (TestKWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
              (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ]) ==
     join [ 0x64e8c3f9ce0f5ba2, 0x63e9777905818a2a, 0x93c8191e7d6e8ae7 ])
```

```cryptol
TestKWAD :
   {a, n}
   (a >= 2, 4 >= a, n >= 2, 2^^54-1 >= n) =>
   [a*64] -> [(n+1)*64] -> (Bit, [n*64])
TestKWAD k ct = (FAIL, pt)
  where
    (FAIL, pt) = KWAD (\c -> AES::decrypt k c) ct

property KWADTests =
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F ])
              (join [ 0x1fa68b0a8112b447, 0xaef34bd8fb5a7b82, 0x9d3e862371d2cfe5 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ])) /\
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
              (join [ 0x96778b25ae6ca435, 0xf92b5b97c050aed2, 0x468ab8a17ad84e5d ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ])) /\
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                      0x1011121314151617, 0x18191A1B1C1D1E1F ])
              (join [ 0x64e8c3f9ce0f5ba2, 0x63e9777905818a2a, 0x93c8191e7d6e8ae7 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ])) /\
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                      0x1011121314151617, 0x18191A1B1C1D1E10 ])
              (join [ 0x28c9f404c4b810f4, 0xcbccb35cfb87f826,
                      0x3f5786e2d80ed326, 0xcbc7f0e71a99f43b,
                      0xfb988b9b7a02dd21 ]) ==
     (True, join [ 0x20ca3cba6f93747e, 0x456c666158ed83da,
                   0x3a6d614e94ba1ac5, 0xfe957c5963100091]))
```

```cryptol
TestTKWAE :
   {n}
   (n >= 2, 2^^28-1 >= n) =>
   [192] -> [n*32] -> [(n+1)*32]
TestTKWAE (k0#k1#k2) pt = ct
  where
    ct = TKWAE (\p -> TDEA::blockEncrypt (k0, k1, k2, p)) pt

property TKWAETests =
    (TestTKWAE 0x12b84c663120c196f8fc17428bc86a110d92cc7c4d3cb695
               0xef7da3da918d0679
            == 0x7a72bbca3aa323aa1ac231ba)
```

```cryptol
TestTKWAD :
   {n}
   (n >= 2, 2^^28-1 >= n) =>
   [192] -> [(n+1)*32] -> (Bit, [n*32])
TestTKWAD (k0#k1#k2) pt = (FAIL, ct)
  where
    (FAIL, ct) = TKWAD (\p -> TDEA::blockDecrypt (k0, k1, k2, p)) pt

property TKWADTests =
    (TestTKWAD 0xe273cd9d7210a973b4113c5772474938d353b54e265dd944
               0x10a38310b604b48f94357d67
            == (False, 0x2ffd56320f1dff99)) /\
    ((TestTKWAD 0x8476d056582e322d93ab9919086798ba48d03eddf77803e5
               0x2c9bf0131cf486402422b8ef).0
            == True)
```

```cryptol
TestKWPAE :
   {a, k, n}
   ( a >= 2, 4 >= a
   , 1 <= k, k < 2^^32
   , 2 <= n, n == 1 + k /^ 8
   ) =>
   [a*64] -> [k][8] -> [n * 64]
TestKWPAE k pt = ct
  where
    ct = KWPAE`{k, n} (\p -> AES::encrypt k p) (join pt)

property KWPAETests =
    (TestKWPAE 0x6decf10a1caf8e3b80c7a4be8c9c84e8
               [0x49]
            == 0x01a7d657fc4a5b216f261cca4d052c2b)

TestKWPAD :
   {a, k, n}
   ( a >= 2, 4 >= a
   , 1 <= k, k < 2^^32
   , 2 <= n, n == 1 + k /^ 8
   ) =>
   [a*64] -> [n * 64] -> (Bit, [k][8])
TestKWPAD k ct = (FAIL, split pt)
  where
    (FAIL, pt) = KWPAD`{k, n} (\c -> AES::decrypt k c) ct

property KWPADTests =
    (TestKWPAD 0x49319c331231cd6bf74c2f70b07fcc5c
               0x9c211f32f8b341f32b052fed5f31a387
            == (False, [0xe4])) /\
    ((TestKWPAD`{k=1} 0x30be7ff51227f0eef786cb7be2482510
                      0x7f61a0a8b2fe7803f2947d233ec3a255).0
            == True) /\
    (TestKWPAD 0x58e7c85b60c7675002bd66e290d20cc694279f0bfc766840
               0xf2edd87dabb4a6ae568662f20fcc4770
            == (False, [0x76])) /\
    ((TestKWPAD`{k=1} 0x94c8dae772a43b5e00468e0947699b239dfe30ab5f90e2f6
                      0x239c6bceee3583fe7825011e02f01cc0).0
            == True)
```

# References

* [NIST Special Publication
  800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final)
  -- "Recommendation for Block Cipher Modes of Operation: Methods for
  Key Wrapping". This document is the primary source material for this
  lab. It contains the original specifications from NIST that we
  develop in this lesson.

* [RFC 3394](https://tools.ietf.org/html/rfc3394) -- "Advanced
  Encryption Standard (AES) Key Wrap Algorithm". This RFC contains
  information on the same set of algorithms and includes test vectors
  which we use in this lab to verify that our algorithms were
  implemented correctly.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Course README](../../README.md) ||
| [< Cryptographic Properties](../CryptoProofs/CryptoProofs.md) | **Key Wrapping (Answers)** | [Capstone >](../LoremIpsum/LoremIpsum.md) |
|| [? Key Wrapping](./KeyWrapping.md) ||
||||
|| [+ Parameterized Modules](../SimonSpeck/SimonSpeck.md) ||

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Cryptol Course ](../../README.md) ||
| [ < Cryptographic Properties ](../CryptoProofs/CryptoProofs.md) | **Key Wrapping** | [ Capstone > ](../LoremIpsum/LoremIpsum.md) |
|| [ ? Key Wrapping ](./KeyWrapping.md) ||
||||
|| [+ Parameterized Modules]( ../SimonSpeck/SimonSpeck.md ) ||
