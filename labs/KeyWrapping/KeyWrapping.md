# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. 

This lab will work through developing the key wrapping algorithms
described in [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) "Recommendation for Block Cipher 
Modes of Operation: Methods for Key Wrapping". We recommend you
have this lab and the specification document open side-by-side.

Here is the abstract from this document:

> This publication describes cryptographic methods that are approved for “key 
> wrapping,” i.e., the protection of the confidentiality and integrity of 
> cryptographic keys. In addition to describing existing methods, this 
> publication specifies two new, deterministic authenticated-encryption modes of
> operation of the Advanced Encryption Standard (AES) algorithm: the AES Key
> Wrap (KW) mode and the AES Key Wrap With Padding (KWP) mode. An analogous mode
> with the Triple Data Encryption Algorithm (TDEA) as the underlying block 
> cipher, called TKW, is also specified, to support legacy applications.

We will develop the three main algorithms -- `KW`, `KWP`, and `TKW` -- by 
building up the necessary subcomponents.

First, since we are creating a new module, the first line needs to be the
module definition:

```
module labs::KeyWrapping::KeyWrapping where
```

# Getting Started

The [NIST Special Publications 800 Series](https://www.nist.gov/itl/publications-0/nist-special-publication-800-series-general-information) provide information of interest to
the computer security community. The series comprises guidelines, 
recommendations, technical specifications, and annual reports of NIST’s 
cybersecurity activities.

Reading through and implementing a formal specification in Cryptol for one of 
the cryptography standards in this series can be a challenge. The standards are 
written by a variety of authors and the algorithms are often described in 
language-agnostic pseudo code which we have to parse. This translation process
can lead to subtle implementation errors which can be hard to spot.

Our job is to extract the relevant details from the specifictation and build
Cryptol specifications for the three main algorithms listed above.


# Preliminaries

The Key Wrapping algorithms described in this document are a form of [Block Cipher Mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation) for an existing
block cipher. Section 5.1 (p. 8) of the standard states:

> For KW and KWP, the underlying block cipher shall be approved, and the block 
> size shall be 128 bits. Currently, the AES block cipher, with key lengths of 
> 128, 192, or 256 bits, is the only block cipher that fits this profile. For 
> TKW, the underlying block cipher is specified to be TDEA, and the block size 
> is therefore 64 bits; the KEK for TKW may have any length for which TDEA is
> approved; see [8].

The algorithms `KW` and `KWP` are defined to operate with [`AES`](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) for the three key sizes: 128, 192, and 256 bits. 

The standard does not indicate a specific key size for [`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), so we will use the **CHECK** standard ??? bit key size.

We will not be developing `AES` or `TDEA` in this lab. Instead we will use
pre-written modules which provide these block ciphers. The algorithms are found
under the `specs/` directory in `specs/Primitive/Symmetric/Cipher/Block`
and we load them into our module with the following:

```
import specs::Primitive::Symmetric::Cipher::Block::AES_parameterized as AES
import specs::Primitive::Symmetric::Cipher::Block::TripleDES as TDEA
```

**TODO** Discuss how to call the encrypt/decrypt functions for these...

Cryptol modules must match the directory structure they reside in. Using
descriptive names as we do here is a good way to organize related algorithms
by type, function, or whatever works for your system.

Now is a good time to scan through the document and get a sense of the overall organization

 * **Sections 1 - 3, Purpose, Authority, and Introduction** -- These sections provide background, usage, and cryptographic function of the algorithms described in this document. This information is good background if we were trying to decide *how* to use these algorithms; however we will not need to reference this information to build our specifications.
 
 Feel free to skim through this material or skip for now.

 * **Section 4, Definitions and Notation** -- This section contains important definitions, acronyms, variables, and operations used in this standard. Let's skim through this to see if we find anything useful...
 
Section `4.3` looks like it has some constants `ICV1`, `ICV2`, and `ICV3` which are defined to have special values. Since we are working inside of our own module we can define these variables without fear of polluting another namespace:

```
ICV1 = 0xA6A6A6A6A6A6A6A6
ICV2 = 0xA65959A6
ICV3 = 0xA6A6A6A6
```

Section `4.4` introduces operators and notation for cryptographic functions and their building blocks. We have imported the required block ciphers, we will be building some of these for ourselves, and the remainder Cryptol provides for us in some fashion or another.

 * **Section 5, Preliminaries** -- **TODO** This is a bit more complicated section about data restrictions.
 
 * **Section 6, Specifications of KW and KWP** -- This section specifies the two primary algorithms `KW` and `KWP`. This section is the reference we will use for the bulk of this lesson.
 
 * **Section 7, Specification of TKW** -- This section specifies the final major algorithm `TKW`. It is structurally very similar to `KW`, but there are some differences that warrant its own section.
 
 * **Section 8, Conformance** -- This section has information about how implementations may claim conformance to the algorithms described in this standard.
 
 
# `KW-AD` and `KW-AE` Specifications

Let's take a look at writing a specification for the `KW-AE` and `KW-AD` algorithms which begin in **Section 6**. We will walk through `KW-AE` and leave `KW-AD` as an exercise for the reader.

The document indicates that `KW-AE` depends on a *wrapping function* `W` (Algorithm 1, page 11). This algorithm has certain *prerequisites* which we will have to model in our formal specification:

  * A *Key Encryption Key (KEK)*, `K` and
  * A *designated cipher function*, `CIPHk`, which operates on 128-bit blocks
  
The document defines a *semiblock* to be a block with half the width of the underlying block cipher. Since we are using `AES` as the underlying block cipher, semiblocks will be 64-bit blocks. Also notice that the specification for `W` defines the *Input* to be a string `S` of `n` semiblocks and the *Output* will be a transformed string `C` of `n` semiblocks. This is enough to build a simple type signature for `W` which will contain the following components:

 * `n` -- A *type parameter* which controls the number of semiblocks in our inputs and outputs 
 * `([128] -> [128])` -- The type of our *keyed* block cipher `CIPHk`
 * `[n][64]` -- The type of our string of input semiblocks
 * `[n][64]` -- The type of our transformed output

**TODO** Add a few words about type parameters?

```
W_prelim1 : {n} (fin n) => ([128] -> [128]) -> [n][64] -> [n][64]
```

We haven't quite captured enough about the type of `W` -- for the algorithm to operate correctly we will have to make two more assumptions about `n`. The first is easy to spot -- the document indicates that `n >= 3`, we can add this restriction to our type signature:

```
W_prelim2 : {n} (fin n, n >= 3) => ([128] -> [128]) -> [n][64] -> [n][64]
```

This *still* isn't quite fully accurate, but it is enough to get started. We can see the final restriction on `n` a bit more clearly after taking a look at implementing `W`.

Taking a close look at Algorithm 1 we can see that `W` transforms our inputs over a series of rounds using the same "step function" which we will call `WStep` in each round. We will make our job easier by modeling this step function first. It may be easier to think about the algorithm by studying `Figure 2` on page 12.

`WStep` will inherit the same type parameters, inputs, and outputs as `W`. We add a new input `t` of type `[64]` which will serve as the round counter.

**Exercise:** Study the diagram in `Figure 2` and finish implementing `WStep` by replacing `FIXME_1`, `FIXME_2`, and `FIXME_3` below with the approprate input for `CIPHk`, and assignments for `A'` and `Rs'`:

```
WStepT :
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> [64] -> [n][64]
WStepT CIPHk ([A] # Rs) t = [A'] # Rs'
  where
    FIXME_1 = zero
    FIXME_2 = zero
    FIXME_3 = zero
    [MSB, LSB] = split (CIPHk (FIXME_1) )
    A'         = FIXME_2
    Rs'        = FIXME_3
```

**TODO** Explain how `([A] # Rs)` automatically assigns values correctly?

 
# The end

How was your experience with this lab? Suggestions are welcome in the form of a 
ticket on the course Github page: [https://github.com/weaversa/cryptol-course/issues](https://github.com/weaversa/cryptol-course/issues)

# References

* [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) -- "Recommendation for Block Cipher Modes of Operation: Methods for Key Wrapping". This document is the primary source material for this lab. It contains the original specifications from NIST that we develop in this lesson.

* [RFC 3394](https://tools.ietf.org/html/rfc3394) -- "Advanced Encryption Standard (AES) Key Wrap Algorithm". This RFC contains information on the same set of algorithms and includes test vectors which we use in this lab to verify that our algorithms were implemented correctly.
