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

**TODO** Moderate expectations, we may only get to a subset for this lab.


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

The standard does not indicate a specific key size for [`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), so we will use the **TODO** standard ??? bit key size.

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
 
 
# Formal Specification of `KW-AE`

Let's take a look at writing a specification for the `KW-AE` and `KW-AD` algorithms which begin in **Section 6**. We will walk through designing a formal specification for `KW-AE` (which we will call `KWAE` in Cryptol because we cannot use the dash/minus sign when naming functions).

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

We haven't quite captured enough about the type of `W` -- for the algorithm to operate correctly we will have to make two more assumptions about `n`.

 * First, `n >= 3`, we can add this restriction to our type signature:
 * Second, `64 >= width (6 * (n - 1))`, we will say more about this later, but it has to do with the appropriate 
 
```
W : {n} (fin n, n >= 3, 64 >= width (6 * (n - 1))) => ([128] -> [128]) -> [n][64] -> [n][64]
```

**TODO** Add a few words about type paremeters and constraints?

Taking a close look at Algorithm 1 we can see that `W` transforms our inputs over a series of rounds using the same "step function" which we will call `WStep` in each round. We will make our job easier by modeling this step function first. It may be easier to think about the algorithm by studying `Figure 2` on page 12.

`WStep` will inherit the same type parameters, inputs, and outputs as `W`. We add a new input `t` of type `[64]` which will serve as the round counter.

**Exercise:** Study the diagram in `Figure 2` and finish implementing `WStep` by replacing `FIXME_1`, `FIXME_2`, and `FIXME_3` below with the approprate input for `CIPHk`, and assignments for `A'` and `Rs'`. 

```
WStep:
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> [64] -> [n][64]
WStep CIPHk ([A] # Rs) t = [A'] # Rs'
  where
    FIXME_1 = zero
    FIXME_2 = zero
    FIXME_3 = zero
    [MSB, LSB] = split (CIPHk (FIXME_1) )
    A'         = FIXME_2
    Rs'        = FIXME_3
```

**TODO** Explain how `([A] # Rs)` automatically assigns values correctly?

Given `WStep` it is a simple matter to complete our definition for `W` we started above. But fist we take a quick aside to recall the `foldl` operator which will come in very handy.

But first...

## A Quick Aside on `foldl`

`foldl` is a Cryptol Primitive and [higher order function](https://en.wikipedia.org/wiki/Higher-order_function) which is useful for (among other things) extracting the final state from some iterative process.

The signature for `foldl` is as follows:

```shell
Cryptol> :t foldl
foldl : {n, a, b} (fin n) => (a -> b -> a) -> a -> [n]b -> a
```

We see that `foldl` takes as inputs the following data

 * A function of type `(a -> b -> a)`  which transforms a "state" of type `a` into a new one by processing an element of type `b`
 * An element of type `a` (an initial state value) and
 * A sequence of elements of type `b`

For instance we can find a list of partial sums of the numbers in the sequence `[1..10]` as follows:

```shell
Cryptol> [0] # [ x + partial | x <- [1..10] | partial <- sums]
[0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
```

However, if we are only interested in the final element of this sequence, then we can use `foldl` as follows:

```shell
Cryptol> foldl (\x -> (\y -> x + y)) 0 [1..10]
55
```

Recall the lambda function notation which is used in the expression `(\x -> (\y -> x + y))` to describe a function taking two parameters and returning their sum.

*...we return to our regularly schedule program.*

We will use `foldl` along with our step function `WStep` to write a definition for `W`.

**Exercise:** Complete the definition of `W` below by replacing the `FIXME` function with an appropriate value to server as an update function. *Hint:* Just plugging in `WStep` won't work. Carefully consider the types at play here. Also recall that you can *partially* evaluate functions -- for an example this, consider the type that `:t (\x -> (\y -> x + y)) 1234` reports in the interpreter.

with the approprate input 
 
```
W :
    {n}
    (fin n, n >= 3, 64 >= width (6 * (n - 1))) =>
    ([128] -> [128]) -> [n][64] -> [n][64]
W CIPHk S = C
  where
    FIXME : [n][64] -> [64] -> [n][64]
    FIXME a b = a
    type s = 6*(n-1)
    t = [ 1 .. s ]
    C = foldl FIXME S t
```

With `W` in hand there isn't much work to do to complete the `KW-AE` algorithm.

```
KWAE :
    {n}
    (fin n, n >= 2, 64 >= width (6 * n)) =>
    ([128] -> [128]) -> [n][64] -> [n+1][64]
KWAE CIPHk P = C
  where
    S = [ICV1] # P
    C = W CIPHk S
```

**TODO** Make a statement about checking your work by testing the test vectors in the section **Test Vectors** below.

# Formal Specification of `KW-AD` 

**TODO** Mostly exercises, discuss choice for the authentication bit and choice of output type

# Test Vectors

Test vectors were not included in NIST-SP-800-38F; however, the `KW-AE`, `KW-AD`
family of key-wrapping algorithm enjoy common usage and are described and
referenced in other standards material. The test vectors in this section
were drawn from [RFC 3394](https://tools.ietf.org/html/rfc3394). 

Recall that you can check individual properties with the `:check` command in the 
interpreter. The properties in this section can be successfully checked and
should report as follows:

```shell
Cryptol> :check RFC3394_TestVector_1
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

And here is the collection of test vectors with harnesses and properties derived
from `RFC3394`.


```
Test_128_128 : [128] -> [128] -> [192]
Test_128_128 k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)
    
TestVector_1_Key = join [ 0x0001020304050607, 
                          0x08090A0B0C0D0E0F ]
TestVector_1_PT  = join [ 0x0011223344556677, 
                          0x8899AABBCCDDEEFF ]
TestVector_1_CT  = join [ 0x1fa68b0a8112b447, 
                          0xaef34bd8fb5a7b82,
                          0x9d3e862371d2cfe5 ]
  
property RFC3394_TestVector_1 = 
  Test_128_128 TestVector_1_Key TestVector_1_PT == TestVector_1_CT
  
    
    
Test_192_128 : [192] -> [128] -> [192]
Test_192_128 k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

TestVector_2_Key = join [ 0x0001020304050607, 
                          0x08090A0B0C0D0E0F,
                          0x1011121314151617 ]
TestVector_2_PT  = join [ 0x0011223344556677, 
                          0x8899AABBCCDDEEFF ]
TestVector_2_CT  = join [ 0x96778b25ae6ca435,
                          0xf92b5b97c050aed2,
                          0x468ab8a17ad84e5d ]

property RFC3394_TestVector_2 = 
  Test_192_128 TestVector_2_Key TestVector_2_PT == TestVector_2_CT



Test_256_128 : [256] -> [128] -> [192]
Test_256_128 k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

TestVector_3_Key = join [ 0x0001020304050607,
                          0x08090A0B0C0D0E0F,
                          0x1011121314151617,
                          0x18191A1B1C1D1E1F ]
TestVector_3_PT  = join [ 0x0011223344556677, 
                          0x8899AABBCCDDEEFF ]
TestVector_3_CT  = join [ 0x64e8c3f9ce0f5ba2,
                          0x63e9777905818a2a,
                          0x93c8191e7d6e8ae7 ]
    
property RFC3394_TestVector_3 = 
  Test_256_128  TestVector_3_Key TestVector_3_PT== TestVector_3_CT
    
    

Test_192_192 : [192] -> [192] -> [256]
Test_192_192 k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

TestVector_4_Key = join [ 0x0001020304050607,
                          0x08090A0B0C0D0E0F,
                          0x1011121314151617 ]
TestVector_4_PT  = join [ 0x0011223344556677,
                          0x8899AABBCCDDEEFF,
                          0x0001020304050607 ]
TestVector_4_CT  = join [ 0x031d33264e15d332,
                          0x68f24ec260743edc,
                          0xe1c6c7ddee725a93,
                          0x6ba814915c6762d2 ]

property RFC3394_TestVector_4 = 
  Test_192_192 TestVector_4_Key TestVector_4_PT == TestVector_4_CT



Test_256_192 : [256] -> [192] -> [256]
Test_256_192 k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

TestVector_5_Key = join [ 0x0001020304050607,
                          0x08090A0B0C0D0E0F,
                          0x1011121314151617,
                          0x18191A1B1C1D1E1F ]
TestVector_5_PT  = join [ 0x0011223344556677,
                          0x8899AABBCCDDEEFF,
                          0x0001020304050607 ]
TestVector_5_CT  = join [ 0xa8f9bc1612c68b3f,
                          0xf6e6f4fbe30e71e4,
                          0x769c8b80a32cb895,
                          0x8cd5d17d6b254da1 ]

property RFC3394_TestVector_5 = 
  Test_256_192 TestVector_5_Key TestVector_5_PT == TestVector_5_CT


Test_256_256 : [256] -> [256] -> [320]
Test_256_256 k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

TestVector_6_Key = join [ 0x0001020304050607,
                          0x08090A0B0C0D0E0F,
                          0x1011121314151617,
                          0x18191A1B1C1D1E1F ]
TestVector_6_CT  = join [ 0x0011223344556677,
                          0x8899AABBCCDDEEFF,
                          0x0001020304050607,
                          0x08090A0B0C0D0E0F ]
TestVector_6_PT  = join [ 0x28c9f404c4b810f4,
                          0xcbccb35cfb87f826,
                          0x3f5786e2d80ed326,
                          0xcbc7f0e71a99f43b,
                          0xfb988b9b7a02dd21 ]

property RFC3394_TestVector_6 = 
  Test_256_256 TestVector_6_Key TestVector_6_CT == TestVector_6_PT
```
 
# The end

How was your experience with this lab? Suggestions are welcome in the form of a 
ticket on the course Github page: [https://github.com/weaversa/cryptol-course/issues](https://github.com/weaversa/cryptol-course/issues)

# References

* [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) -- "Recommendation for Block Cipher Modes of Operation: Methods for Key Wrapping". This document is the primary source material for this lab. It contains the original specifications from NIST that we develop in this lesson.

* [RFC 3394](https://tools.ietf.org/html/rfc3394) -- "Advanced Encryption Standard (AES) Key Wrap Algorithm". This RFC contains information on the same set of algorithms and includes test vectors which we use in this lab to verify that our algorithms were implemented correctly.