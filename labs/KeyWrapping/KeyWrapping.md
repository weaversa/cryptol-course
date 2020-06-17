# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. 

This lab will take the student through developing wrapping algorithms
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

In this lab we will focus on developing two of the three main algorithms -- `KW`
and `TKW` -- by building up the necessary subcomponents. In fact each of
these is a family of algorithms: `KW` is composed of an 
*authenticated encryption* component `KW-AE` and an *authenticated decryption*
component `KW-AD`; similarly `TKW` is composed of `TKW-AE` and `TKW-AD`. The
`KWP` padded family will appear as the subject of a future lab.

Since we are creating a new module, the first line needs to be the
module definition:

```
module labs::KeyWrapping::KeyWrapping where
```

# Preliminaries

The [NIST Special Publications 800 Series](https://www.nist.gov/itl/publications-0/nist-special-publication-800-series-general-information) provides information of interest to
the computer security community. The series comprises guidelines, 
recommendations, technical specifications, and annual reports of NIST’s 
cybersecurity activities.

Reading through and implementing a formal specification in Cryptol for one of 
the cryptography standards in this series can be a challenge. The standards are 
written by a variety of authors and the algorithms are often described in 
language-agnostic pseudo code which we have to parse. This translation process
can lead to subtle implementation errors or potentially false assumptions about 
the algorithms which can be hard to spot.

Our job is to extract the relevant details from the specifictation and build
Cryptol specifications for the three main algorithms listed above.


# Getting Started

The Key Wrapping algorithms described in this document are a form of [Block Cipher Mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation) for an existing
block cipher. Section 5.1 (p. 8) of the standard indicates:

> For KW and KWP, the underlying block cipher shall be approved, and the block 
> size shall be 128 bits. Currently, the AES block cipher, with key lengths of 
> 128, 192, or 256 bits, is the only block cipher that fits this profile. For 
> TKW, the underlying block cipher is specified to be TDEA, and the block size 
> is therefore 64 bits; the KEK for TKW may have any length for which TDEA is
> approved; see [8].

`KW` (and `KWP`) are defined to operate with [`AES`](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) for the three key sizes: 128, 192, and 256 bits. 

The standard does not indicate a specific key size for [`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), but `TDEA`/Triple-DES is typically used with a 192 bit key and that is what we will develop later in this lab.

Also, we will not develop `AES` or `TDEA` in this lab. Instead we will use
pre-written modules which provide these block cipher primitives. The algorithms
are found under the `specs/` directory in `specs/Primitive/Symmetric/Cipher/Block`
and we load them into our module with the following:

```
import specs::Primitive::Symmetric::Cipher::Block::AES_parameterized as AES
import specs::Primitive::Symmetric::Cipher::Block::TripleDES as TDEA
```

Cryptol modules must match the directory structure they reside in. Using
descriptive names as we do here is a good way to organize related algorithms
by type, function, or whatever works for your system.

Now is a good time to scan through the document and get a sense of the overall organization:

 * **Sections 1 - 3, Purpose, Authority, and Introduction** -- These sections provide background, usage, and cryptographic function of the algorithms described in this document. This information is good background if we were trying to decide *how* to use these algorithms; however we will not need to reference this information to build our specifications.
 
 Feel free to skim through this material or skip for now.

 * **Section 4, Definitions and Notation** -- This section contains important definitions, acronyms, variables, and operations used in this standard. Let's scan through this to see if we find anything useful...
 
Section `4.3` looks like it has some constants `ICV1`, `ICV2`, and `ICV3` which are defined to have special values. Since we are working inside of our own module we can define these variables without fear of polluting another namespace:

```
ICV1 = 0xA6A6A6A6A6A6A6A6
ICV2 = 0xA65959A6
ICV3 = 0xA6A6A6A6
```

Section `4.4` introduces operators and notation for cryptographic functions and their building blocks. We have already imported the required block ciphers and we will be building some of these for ourselves. For the remainder, Cryptol provides analogous functionality to us in some fashion or another.

 * **Section 5, Preliminaries** -- **TODO** This section covers usage and information about data size restrictions.
 
 * **Section 6, Specifications of KW and KWP** -- This section specifies the two families of algorithms `KW` and `KWP`. This section is the reference we will use for the bulk of this lab as we work through building a specification for `KW`.
 
 * **Section 7, Specification of TKW** -- This section specifies the final major algorithm `TKW`. It is structurally very similar to `KW`, but there are some differences that warrant its own section in the standards. The algorithms here
 will be left as an exercise for the reader at the end of this lab.
 
 * **Section 8, Conformance** -- This section has information about how implementations may claim conformance to the algorithms described in this standard.
 
 
# Formal Specification of `KW`

`KW` is a family of algorithms comprised of `KW-AE` and `KW-AD` we start with
`KW-AE`.

## Building a Formal Specification for `KW-AE`

Let's take a look at writing a specification for the `KW-AE` which is presented
in **Section 6**. We will walk through designing a formal specification for `KW-AE` (which we will call `KWAE` in Cryptol because we cannot use the dash/minus sign when naming functions).

The document indicates that `KW-AE` depends on a *wrapping function* `W` (Algorithm 1, page 11). This algorithm has certain *prerequisites* which we will have to model in our formal specification:

  * A **Key Encryption Key (KEK)** `K` and
  * A **designated cipher function** `CIPHk`, which operates on 128-bit blocks
  
The document defines a *semiblock* to be a block with half the width of the underlying block cipher. Since we are using `AES` as the underlying block cipher, semiblocks will be 64-bit blocks. Also notice that the specification for `W` defines the *Input* to be a string `S` of `n` semiblocks and the *Output* will be a transformed string `C` of `n` semiblocks. This is enough to build a simple type signature for `W` which will contain the following components:

 * `n` -- A *type parameter* which controls the number of semiblocks in our inputs and outputs 
 * `([128] -> [128])` -- The type of our *keyed* block cipher `CIPHk`
 * `[n][64]` -- The type of our string of input semiblocks
 * `[n][64]` -- The type of our transformed output

Putting these together we have our preliminary type signature:

```ignore
W_prelim : 
  {n} 
  (fin n) => 
  ([128] -> [128]) -> [n][64] -> [n][64]
```

We haven't quite captured enough about the type of `W` -- for the algorithm to operate correctly, and according to the standard, we will have to make two more assumptions about `n`.

 * First, `n >= 3`, we can add this restriction to our type signature:
 * Second, `64 >= width (6 * (n - 1))`, we will say more about this later, but it has to do with the data size restrictions found in **Section 5**.
 
```ignore
W : 
  {n} 
  (fin n, n >= 3, 64 >= width (6 * (n - 1))) => 
  ([128] -> [128]) -> [n][64] -> [n][64]
```

Taking a close look at `Algorithm 1` we can see that `W` transforms our inputs over a series of rounds using the same "step function", which we will call `WStep`, in each round. It will make our job easier to model this step function first. Also, it may be easier to think about the algorithm by studying `Figure 2` on page 12.

`WStep` will inherit the same type parameters, inputs, and outputs as `W`. We add a new input `t` of type `[64]` which serves as the round counter found in step 2 of
`Algorithm 1`.

**Exercise:** Study `Algorithm 1` and `Figure 2`. Finish implementing `WStep` by replacing `FIXME_1`, `FIXME_2`, and `FIXME_3` below with the approprate input for `CIPHk`, and assignments for `A'` and `Rs'` found in the body of the function.

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

*Note:* A shorthand is used for one of the parameters to `WStep`. According to
the type signature, the second parameter is of type `[n][64]` -- a sequence of
semiblocks. In the definition of `WStep` we see that this parameter is identified
as `([A] # Rs)`. Cryptol assigns the *first* semiblock to `A` and all the
remaining semiblocks to `Rs`. Since we have the type parameter condition `n >= 3`
We know that there are at least three such blocks to assign, and at least two
will be assigned to `Rs`.

Given `WStep` it is a simple matter to complete our definition for `W` we started above. But first we take a quick aside to recall the `foldl` operator which will come in very handy.

*But first...*

### A Quick Aside on `foldl`

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

One application for `foldl` is to access the final element of some iterative 
process. For instance, we can find a list of partial sums from the sequence `[1..10]` as follows:

```shell
Cryptol> [0] # [ x + partial | x <- [1..10] | partial <- sums]
[0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
```

However, if we are only interested in the final element of this sequence, then we can use `foldl` as follows:

```shell
Cryptol> foldl (\x -> (\y -> x + y)) 0 [1..10]
55
```

Here we use lambda function notation to describe the update function. `(\x -> (\y -> x + y))` is a function taking two parameters `x` and `y` and returns their sum.

*...now we return to our regularly schedule program.*

We will use `foldl` along with our step function `WStep` to write a definition for `W`.

**Exercise:** Complete the definition of `W` below by replacing the `FIXME` function with an appropriate value to serve as an update function. *Hint:* Just plugging in `WStep` won't work. What is the type of the first parameter for `foldl`? Also recall that you can *partially* evaluate functions. For example, consider what `:t (\x -> (\y -> x + y)) 1234` reports in the interpreter. What would that partially evaluated function *do*?

"Hint: Just plugging in WStep won't work. How many parameters does WStep take? Right now,  FIXME has two parameters (a and b)..."
 
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

With `W` in hand there it isn't much more work to complete the `KW-AE` algorithm.

**Exercise:** Study `Algorithm 3` and complete the specification for `KW-AE` by replacing `FIXME` in the snippet below with the correct definition for `S`. 

```
KWAE :
    {n}
    (fin n, n >= 2, 64 >= width (6 * n)) =>
    ([128] -> [128]) -> [n][64] -> [n+1][64]
KWAE CIPHk P = C
  where
    FIXME = zero:[n+1][64]
    S = FIXME
    C = W CIPHk S
```

At this point you can check your solutions with some test vectors defined by
properties later on in this document. Here is the the command and sample output 
for `RFC3394_TestVector_1`. Tests `RFC3394_TestVector_1` through 
`RFC3394_TestVector_6` should succeed similarly:

```shell
Cryptol> :check RFC3394_TestVector_1
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Building a Formal Specification for `KW-AD`

It should be fairly straightforward at this point to implement the authenticated
decryption function `KW-AD` which is the other algorithm in the `KW` family. There is one significant difference, though: since the algorithm *authenticates* the decryption we will need to add an authentication check and slightly alter the type for `KW-AD`.

**Exercise:** Review `Algorithm 2` and `Figure 3` of the document and complete 
the definitions for the inverse routines to `W` and `WStep` which we shall call
`W'` and `WStep'` respectively. The type declarations for these functions are
provided. 

*Hint:* Notice that, except for the names, the type declarations are identical.
The function definitions are also very similar. Pay special attention
to the order of the index variable for the main loop, the sequence of operations,
and how the sequence of `Rs` transforms:

```
W' :
    {n}
    (fin n, n >= 3, 64 >= width (6 * (n - 1))) =>
    ([128] -> [128]) -> [n][64] -> [n][64]
W' CIPHk' C = zero

WStep' :
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> [64] -> [n][64]
WStep' CIPHk ([A] # Rs) t = zero
```

Once you have these completed you shoudl be able to check your work by having 
Cryptol `:prove` the properies `WStepInvProp` and `WInvProp`. Your output should look something like the following:

```shell
Cryptol> :prove WStepInvProp 
Q.E.D.
(Total Elapsed Time: 0.079s, using Z3)
Cryptol> :prove WInvProp
Q.E.D.
(Total Elapsed Time: 1.832s, using Z3)
```

Here are the definitions of these properties:

```
property WStepInvProp ARs t =
    WStep'`{3} (\a -> a-1) (WStep (\a -> a+1) ARs t) t == ARs

property WInvProp S =
    W'`{3} (\a -> a-1) (W (\a -> a+1) S) == S
```


The final step is to use these components to write the authenticated decryption
algorithm `KW-AD`. Unlike `W'` and `WStep'` this function will have a different
type than its related routine in the `KW-AE` family becase it needs to capture
whether or not the ciphertext authenticates *as well as* computing the
corresponding plaintext.

**Exercise:** Study `Algorithm 4` from the standard, and complete the 
definition of `KWAD` below by replacing `FIXME_1`, `FIXME_2`, and `FIXME_3`
with appropriate definition fors `S`, a computation of the authentication bit to assign to `FAIL`, and finally the appropriate plaint text. 

Notice that `FAIL` indicates a *failure* to authenticate so should be `False` for authenticated decryptions and `True` for failures to authenticate.

*Hint:* Review the Cryptol primitives `head` and `tail` after you think you have
the appropriate definition for `S`/`FIXME_1`

```
KWAD :
    {n}
    (fin n, n >= 3, 64 >= width (6 * (n-1))) =>
    ([128] -> [128]) -> [n][64] -> (Bit, [n-1][64])
KWAD CIPHk' C = (FAIL, P)
  where
    FIXME_1 = zero:[n][64]
    FIXME_2 = zero:Bit
    FIXME_3 = zero:[n-1][64]
    S = FIXME_1
    FAIL = FIXME_2
    P = FIXME_3
```

When you have successfully defined this function, you can test your work by
`:prove`ing that the following properties are true for 128, 192 and 256 bit key sizes."

```
property KWAEInvProp S = 
    KWAD`{3} (\a -> a-1) (KWAE (\a -> a+1) S) == (False, S)

//KW with AES128
KWAE128 (k : [128]) pt = join (KWAE`{2} (AES::encrypt k) (split pt))

KWAD128 (k : [128]) ct = (FAIL, join pt)
  where (FAIL, pt) = KWAD`{3} (AES::decrypt k) (split ct)

property KWAE128Test = KWAD128 1234 (KWAE128 1234 5678) == (False, 5678)


//KW with AES192
KWAE192 (k : [192]) pt = join (KWAE`{3} (AES::encrypt k) (split pt))

KWAD192 (k : [192]) ct = (FAIL, join pt)
  where (FAIL, pt) = KWAD`{4} (AES::decrypt k) (split ct)

property KWAE192Test = KWAD192 1234 (KWAE192 1234 5678) == (False, 5678)

//KW with AES256
KWAE256 (k : [256]) pt = join (KWAE`{4} (AES::encrypt k) (split pt))

KWAD256 (k : [256]) ct = (FAIL, join pt)
  where (FAIL, pt) = KWAD`{5} (AES::decrypt k) (split ct)

property KWAE256Test = KWAD256 1234 (KWAE256 1234 5678) == (False, 5678)
```

For instance running `:prove KWAE256Test` in the interpreter should result in
output that looks like this:

```shell
Cryptol> :prove KWAE256Test
Q.E.D.
(Total Elapsed Time: 0.005s, using Z3)
```

# Formal Specification of `TKW-AE` and `TKW-AD`

**Exercise:** Try your hand at writing the specification for `TKW-AE` and 
`TKW-AD`. These functions are very similar to `KW-AE` and `KW-AD` but they use
the 64-bit block cipher `TDEA` (also known as 
[Triple-DES](https://en.wikipedia.org/wiki/Triple_DES)). We recommend following
the pattern above and defining the helper functions `TWStep`, `TWStep'`, `TW`, and
`TW'` before you attempt `TKW-AE` and `TKW-AD`.

There are some minor modifications to be made, but things should go easily
using `KW-AE` and `KW-AD` as a reference. This is a good opportunity
to go back through and take a close look at the type parameters and conditions
and be sure you understand what they mean and how to use them.

One important difference in the `TKW` family is you will use the Triple-DES algorithm that's implemented in the TDEA module we imported earlier. Check it's type with `:t TDEA::blockEncrypt`, this
has a slightly different interface than the block cipher we used from the
`AES` module earlier. 

You can define a keyed TDEA instance as follows:

```
[K1, K2, K3] = [0x0123456789ABCDEF, 0x23456789ABCDEF01, 0x456789ABCDEF0123]
TDEA_keyed = (\p -> TDEA::blockEncrypt (K1, K2, K3, p))
```

Furthermore, we can check that this cipher was loaded correctly by checking the 
property `TDEA::testsPass` defined in `TripleDES.cry` as follows:

```shell
:prove TDEA::testsPass
Q.E.D.
(Total Elapsed Time: 0.006s, using Z3)
```

It is worth taking a look through the `TripleDES.cry` and learn about the story
of a particularly famous NIST test vector.

Good luck!


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

**Exercise:** Look at the test vectors provided by NIST which are included in the `kwtestvectors`. Use these to create a set of test vectors and properties for various input and key sizes to further test the algorithms we've developed in 
this lab. You will also find vectors for `TKW` there which you can use to check
your implementations in the exercise above.

Feel free to model your properties after the pattern for the `RFC3394` test 
vectors we include here:

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
