# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. 

This lab will take the student through developing wrapping algorithms
described in [NIST Special Publication
800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final)
"Recommendation for Block Cipher Modes of Operation: Methods for Key
Wrapping". We recommend you have this lab and the specification
document open side-by-side.

Here is the abstract from this document:

> This publication describes cryptographic methods that are approved
> for “key wrapping,” i.e., the protection of the confidentiality and
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

Since we are creating a new module, the first line needs to be the
module definition:

```
module labs::KeyWrapping::KeyWrapping where
```

# Preliminaries

The [NIST Special Publications 800
Series](https://www.nist.gov/itl/publications-0/nist-special-publication-800-series-general-information)
provides information of interest to the computer security
community. The series comprises guidelines, recommendations, technical
specifications, and annual reports of NIST’s cybersecurity activities.

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
for its three key sizes: 128, 192, and 256-bits.

The standard does not indicate a specific key size for
[`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), but
`TDEA`/Triple-DES is typically used with a 192-bit key and that is
what we will develop later in this lab.

Also, we will not develop `AES` or `TDEA` in this lab. Instead we will
use pre-written modules which provide these block cipher
primitives. The algorithms are found under the `specs/` directory in
`specs/Primitive/Symmetric/Cipher/Block` and we import them into our
module with the following:

```
import specs::Primitive::Symmetric::Cipher::Block::AES_parameterized as AES
import specs::Primitive::Symmetric::Cipher::Block::TripleDES as TDEA
```

Cryptol modules must match the directory structure they reside
in. Using descriptive names as we do here is a good way to organize
related algorithms by type, function, or whatever works for your
system.

Now is a good time to scan through the document and get a sense of the
overall organization:

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
 
Section `4.3` provides some constants `ICV1`, `ICV2`, and `ICV3` which
are defined to have special values. Since we are working inside of a
module we can define these variables without fear of polluting another
namespace by placing them in a `private` code block:

```
private
    ICV1 = 0xA6A6A6A6A6A6A6A6
    ICV2 = 0xA65959A6
    ICV3 = 0xA6A6A6A6
```

Section `4.4` introduces operators and notation for cryptographic
functions and their building blocks. We have already imported the
required block ciphers and we will be building some of these for
ourselves. For the remainder, Cryptol provides analogous functionality
to us in some fashion or another.

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

`KW` is a family of algorithms comprised of `KW-AE` and `KW-AD` we
start with `KW-AE`.

## Building a Formal Specification for `KW-AE`

Let's take a look at writing a specification for the `KW-AE` which is
presented in **Section 6**. We will walk through designing a formal
specification for `KW-AE` (which we will call `KWAE` in Cryptol
because we cannot use the dash/minus sign when naming functions).

The document indicates that `KW-AE` depends on a *wrapping function*
`W` (Algorithm 1, page 11). This algorithm has certain *prerequisites*
which we will have to model in our formal specification:

  * A **Key Encryption Key (KEK)** `K` and
  * A **designated cipher function** `CIPHk`, which operates on 128-bit blocks
  
The document defines a *semiblock* to be a block with half the width
of the underlying block cipher. Since we are using `AES` as the
underlying block cipher, semiblocks will be 64-bit blocks. Also notice
that the specification for `W` defines the *Input* to be a string `S`
of `n` semiblocks and the *Output* will be a transformed string `C` of
`n` semiblocks. This is enough to build a simple type signature for
`W` which will contain the following components:

 * `n` -- A *type parameter* which controls the number of semiblocks
   in our inputs and outputs
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

We haven't quite captured enough about the type of `W` -- for the
algorithm to operate correctly, and according to the standard, we will
have to make two more assumptions about `n`.

 * First, `n >= 3`, we can add this restriction to our type signature:
 * Second, `n <= 2^^54`, this comes directly limits imposed by Table 1
   (Section 5.3.1, page 10). As an aside, if this constraint is left
   off, Cryptol's type checker will point out that `64 >= width
   (6*(n-1))` (which can be reexpressed as `6*(n-1) < 2^^64`). This
   constraint comes from the fact that the value `t` (which iterates
   over `[1..6*(n-1)]`) has to fit into a 64-bit word when passed to
   `WStep`. Of course, `2^^54` is less than `6 * (2^^54 - 1)` which is
   less than `2^^64`, so the tighter lower bound from Table 1 is
   acceptable.
 
```ignore
W : 
  {n} 
  (fin n, 3 <= n, n <= 2^^54) =>
  ([128] -> [128]) -> [n][64] -> [n][64]
```

Taking a close look at `Algorithm 1` we can see that `W` transforms
our inputs over a series of rounds using the same "step function",
which we will call `WStep`, in each round. It will make our job easier
to model this step function first. Also, it may be easiest to
understand this step function by studying `Figure 2` on page 12.

`WStep` will inherit the same type parameters, inputs, and outputs as
`W`. We add a new input `t` of type `[64]` which serves as the round
counter found in step 2 of `Algorithm 1`.

**EXERCISE**: Study `Algorithm 1` and `Figure 2`. Implement WStep
  below assuming the appropriate input for `CIPHk` and assignments for
  `A'` and `Rs'` found in the body of the function.

```
WStep:
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> [64] -> [n][64]
WStep CIPHk ([A] # Rs) t = [A'] # Rs'
  where
    [MSB, LSB] = split (CIPHk (A # head Rs))
    A'         = MSB ^ t
    Rs'        = tail Rs # [LSB]
```

*Note:* Pattern matching (a kind of shorthand) is used for one of the
parameters to `WStep`. According to the type signature, the second
parameter is of type `[n][64]` -- a sequence of semiblocks. In the
definition of `WStep` we see that this parameter is identified as
`([A] # Rs)`. Cryptol assigns the *first* semiblock to `A` and all the
remaining semiblocks to `Rs`. Since we have the type parameter
condition `n >= 3` We know that there are at least three such blocks
to assign, and at least two will be assigned to `Rs`.

Given `WStep` it is a simple matter to complete our definition for `W`
we started above. But first we take a quick aside to recall the
`foldl` operator which will come in very handy.

*But first...*

### A Quick Aside on `foldl`

`foldl` is a Cryptol Primitive and [higher order
function](https://en.wikipedia.org/wiki/Higher-order_function) which
is useful for (among other things) extracting the final state from
some iterative process.

The signature for `foldl` is as follows:

```shell
Cryptol> :t foldl
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

```shell
Cryptol> [0] # [ x + partial | x <- [1..10] | partial <- sums]
[0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
```

However, if we are only interested in the final element of this
sequence, then we can use `foldl` as follows:

```shell
Cryptol> foldl (+) 0 [1..10]
55
```
*...we now return to our regularly schedule program.*

We will use `foldl` along with our step function `WStep` to write a
definition for `W`.

**EXERCISE**: Complete the definition of `W` below by filling in the
  function skeleton provided. *Hint:* Folding over vanilla `WStep`
  won't work -- really consider the type of the first argument to
  `foldl` and you'll see you need to partially evaluate WStep on CIPHk
  first.
 
```
W :
    {n}
    (fin n, 3 <= n, n <= 2^^54) =>
    ([128] -> [128]) -> [n][64] -> [n][64]
W CIPHk S = C
  where
    type s = 6*(n-1)
    ts     = [ 1 .. s ]
    C      = foldl (WStep CIPHk) S ts
```

With `W` in hand there it isn't much more work to complete the `KW-AE`
algorithm.

**EXERCISE**: Study `Algorithm 3` and complete the specification for
  `KW-AE` by filling in the snippet below with the correct definition
  for `S`. Remember that `ICV1` has already been defined in this
  module. Also, notice that the bounds from Table 1 (Section 5.3.1,
  page 10) are included as type constraints.

```
KWAE :
    {n}
    (fin n, 2 <= n, n < 2^^54) =>
    ([128] -> [128]) -> [n][64] -> [n+1][64]
KWAE CIPHk P = C
  where
    S = [ICV1] # P
    C = W CIPHk S
```

At this point you can check your work against six test vectors given
in a property defined later on in this document. Here is the the
command and sample output for `KWAE_RFC3394_Tests`.

```shell
Cryptol> :check KWAE_RFC3394_Tests
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
complete the definitions for the inverse routines to `W` and `WStep`
which we shall call `W'` and `WStep'` respectively. The type
declarations for these functions are provided.

*Hint:* Notice that, except for the names, the type declarations are
identical. The function definitions are also very similar. Pay
special attention to the order of the index variable for the main
loop, the sequence of operations, and how the sequence of `Rs`
transforms:

```
WStep' :
    {n}
    (fin n, n >= 3) =>
    ([128] -> [128]) -> [n][64] -> [64] -> [n][64]
WStep' CIPHk' ([A] # Rs) t = [A'] # Rs'
  where
    [MSB, LSB] = split (CIPHk' ((A ^ t) # last Rs))
    A'         = MSB
    Rs'        = [LSB] # take Rs

W' :
    {n}
    (fin n, 3 <= n, n <= 2^^54) =>
    ([128] -> [128]) -> [n][64] -> [n][64]
W' CIPHk' C = S
  where
    type s = 6*(n-1)
    ts     = [ s, s-1 .. 1 ]
    S      = foldl (WStep' CIPHk') C ts
```

Once you have these completed you should be able to check your work by
having Cryptol `:prove` the properties `WStepInvProp` and
`WInvProp`. Your output should look something like the following:

```shell
Cryptol> :prove WStep'Prop 
Q.E.D.
(Total Elapsed Time: 0.079s, using Z3)
Cryptol> :prove W'Prop
Q.E.D.
(Total Elapsed Time: 1.832s, using Z3)
```

These two properties state that for a fixed, dummy CIPHk and S of
length 3 semiblocks, `WStep` and `WStep'` are inverses and `W` and
`W'` are inverses. Here are the definitions of these properties:

```
property WStep'Prop ARs t =
    WStep'`{3} (\a -> a-1) (WStep (\a -> a+1) ARs t) t == ARs

property W'Prop S =
    W'`{3} (\a -> a-1) (W (\a -> a+1) S) == S
```

The final step is to use these components to write the authenticated
decryption algorithm `KW-AD`. Unlike `W'` and `WStep'` this function
will have a different type than its related routine in the `KW-AE`
family because it needs to capture whether or not the ciphertext
authenticates *as well as* compute the corresponding plaintext.

**EXERCISE**: Study `Algorithm 4` from the standard and complete the
  definition of `KWAD` below by by filling in the function skeleton
  provided. This function needs both an appropriate definition for
  `S`, a computation of the authentication bit to assign to `FAIL`,
  and finally the appropriate plaintext.

Notice that `FAIL` indicates a *failure* to authenticate so should be
`False` for authenticated decryptions and `True` for failures to
authenticate.

*Hint:* Review the Cryptol primitives `head` and `tail`.

```
KWAD :
    {n}
    (fin n, 2 <= n, n < 2^^54) =>
    ([128] -> [128]) -> [n+1][64] -> (Bit, [n][64])
KWAD CIPHk' C = (FAIL, P)
  where
    S = W' CIPHk' C
    FAIL = head S != ICV1
    P = tail S
```

When you have successfully defined this function, you can test your
work by `:prove`ing that `KWAE` and `KWAD` are inverses (well, at
least for a dummy CIPHk and P of length 3 semiblocks) using the
`KWAEInvProp`. You can also check your work against six test vectors
by using the property `KWAD_RFC3394_Tests` (this is defined later on
in this document).

```
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
  recommend follwing the same steps from above and defining the helper
  functions `TWStep`, `TWStep'`, `TW`, and `TW'` before you attempt
  `TKW-AE` and `TKW-AD`. Test vectors are available in the
  [kwtestvectors directory](kwtestvectors).

There are some minor modifications to be made, but things should go
easily using `KW-AE` and `KW-AD` as a reference. This is a good
opportunity to go back through and take a close look at the type
parameters and conditions and be sure you understand what they mean
and how to use them. One important thing to note is that had we
defined our functions above using a `semigroup` type parameter (rather
than hard-code 64), our work defining the TKW family of functions
would already be done!

One important difference in the TKW family is you will use the
Triple-DES algorithm that's implemented in the `TDEA` module we imported
earlier. You can check the type of `TDEA` in the interpreter via `:t
TDEA::blockEncrypt`. This has a slightly different interface than the
block cipher we used from the `AES` module earlier.

You can define a keyed `TDEA` instance as follows:

```
[K1, K2, K3] = [0x0123456789ABCDEF, 0x23456789ABCDEF01, 0x456789ABCDEF0123]
TDEA_keyed = (\p -> TDEA::blockEncrypt (K1, K2, K3, p))
```

It is worth taking a quick look through the `TripleDES.cry` to learn a
little bit about a particularly famous NIST test vector.

Good luck!

# A Formal Specification of `KWP-AE`

`KWP-AE` is the authenticated-encryption function and makes use of our
previously defined `W`. There are two new concepts to introduce with
this specification, so we'll break `KWP-AE` into two parts. The first concept is that of **padding**.

## Padding

Many cryptographic specifications (especially hash functions) accept
some arbitrary number of bits as input but operate over some number of
words internally. Hence, it's common to see a bitvector **padded**
with zeros (or sometimes a constant and zeroes) to inflate the
bitvector until its size is a multiple of a word (usually 32- or
64-bits). For example, say we have a bitvector of `37` bits and we
want to pad it to fit into some number of 32-bit words. Well, the next
largest multiple of `32` is `64`, and `64 - 37` is `27`, so we'll need
to pad with `27` zeros. We can demonstrate this using Cryptol:

```
bits = 0b1001100101110011111010000001110011011 : [37]
bits_padded = bits # (0 : [27]) : [64]
```

However, what if we need to create a function that pads any size input
into a bitvector with a size that is a multiple of 32? In general, the
pad is what’s needed to make something a multiple of a
word. Mathematically, this is identical to the **ceiling
modulus**. Backing up a second -- `a % b` gives the amount of `a`
remains after dividing by `b` (`%` is Cryptol's [remainder (or modulo)
operator](https://en.wikipedia.org/wiki/Modulo_operation)). Consider
we have `10` apples divided amongst `3` friends, after giving everyone
three apples, we'll have `10 % 3 = 1` apple remaining. What we desire
with padding isn't what's remaining, but rather the amount needed to
get to the next multiple, that is, how many **more** apples do we need
if we had another friend? --- To which the answer here is, `2`. Or,
said another way, we are **short** `2` apples.

As it turns out, Cryptol has such a shortage operator (the ceiling
modulus), namely, `%^`

```sh
labs::KeyWrapping::KeyWrapping> :h (%^)

    primitive type (%^) : # -> # -> #

    `m %^ n` requires:
      • fin m
      • fin n
      • n >= 1

Precedence 90, associates to the left.

How much we need to add to make a proper multiple of the second argument.
```

So, to revisit our padding example above, if we have a bitvector of
length `37` and it needs to be padded to a multiple of `32`, we're
short `27`, as demonstrated here using Cryptol:

```sh
labs::KeyWrapping::KeyWrapping> `(37 %^ 32)
27
```

Now we can revisit creating a function that pads any size input
into a bitvector with a size that is a multiple of 32.

```
pad32 : {a} (fin a) => [a] -> [a + a %^ 32]
pad32 x = x # 0
```

Here we take in a bitvector `x` of length `a` bits and produce a
bitvector of length `a + a %^ 32` bits by concatenating `a %^ 32` zero
bits onto `x`.


## KWP-AE Padding

The first 4 steps of `KWP-AE` describe how to create `S` from `P`.
Here we introduce the type variable `k` to be the number of octets (or
bytes) of `P`, and `l` to be the number of bits of `S`. You'll notice
we constrain `k` to be between `1` and `2^^32-1`, as per Table 1.
Since `S` is comprised of two 32-bit numbers concatenated with input
`P` and then padded to a multiple of `64`, `l` is constrained to be
`32 + 32 + k*8 + k*8 %^ 64`, that is, 32-bits for `ICV2` plus 32-bits
for the number of octets of `P` plus `P` itself plus the shortage of
`P` as a multiple of `64` bits.

You may notice that the specification document gives the number of
octets to pad as

> ![](https://render.githubusercontent.com/render/math?math=8\cdot\lceil%20len(P)/64\rceil%20-len(P)/8)

In general,
![](https://render.githubusercontent.com/render/math?math=b\cdot\lceil%20a/b\rceil%20-a=a\%25\hat{}b),
and since Cryptol supports the more concise shortage operator, we'll
use that instead.



```
KWPAEPad :
    {k, l}
    ( 1 <= k, k < 2^^32  // Bounds on the number of octets of P, from Table 1
    , l == 32 + 32 + k*8 + k*8 %^ 64  // The type of S
    ) =>
    [k][8] -> [l]
KWPAEPad P = ICV2 # (`k : [32]) # (join P) # zero
```


```
KWPAE :
    {k, l, n}
    ( 1 <= k, k < 2^^32  // Bounds on the number of octets of P, from Table 1
    , l == 32 + 32 + k*8 + k*8 %^ 64  // The type of S and C
    , 3 <= n, n <= 2^^54              // Bounds on n, from W
    , 64*n == max 192 l               // Relate n and l
    ) =>
    ([128] -> [128]) -> [k][8] -> [l]
KWPAE CIPHk P = C
  where
    S          = KWPAEPad P
    C          = if (`k : [32]) <= 8 then
                   widen (CIPHk (shrink S))
                 else
                   shrink (join (W`{n} CIPHk (split (widen S))))

widen : {a, b} (fin a, fin b) => [b] -> [a + b]
widen a = 0 # a

shrink : {a, b} (fin a, fin b) => [a + b] -> [b]
shrink a = drop a
```

# Test Vectors

Test vectors were not included in NIST-SP-800-38F; however, the
`KW-AE`, `KW-AD` family of key-wrapping algorithm enjoy common usage
and are described and referenced in other standards material. The test
vectors in this section were drawn from [RFC
3394](https://tools.ietf.org/html/rfc3394).

Recall that you can check individual properties with the `:check`
command in the interpreter.  Here are some test vectors from [RFC
3394](rfc3394.pdf) that are useful for testing `KW-AE` and `KW-AD`.

```
Test_KWAE :
   {a, n}
   (fin a, a >= 2, 4 >= a, n >= 2, 2^^54-1 >= n) =>
   [a*64] -> [n*64] -> [(n+1)*64]
Test_KWAE k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

property KWAE_RFC3394_Tests =
    (Test_KWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ]) ==
     join [ 0x1fa68b0a8112b447, 0xaef34bd8fb5a7b82, 0x9d3e862371d2cfe5 ]) /\
    (Test_KWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ]) ==
     join [ 0x96778b25ae6ca435, 0xf92b5b97c050aed2, 0x468ab8a17ad84e5d ]) /\
    (Test_KWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ]) ==
     join [ 0x64e8c3f9ce0f5ba2, 0x63e9777905818a2a, 0x93c8191e7d6e8ae7 ]) /\
    (Test_KWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ]) ==
     join [ 0x031d33264e15d332, 0x68f24ec260743edc,
            0xe1c6c7ddee725a93, 0x6ba814915c6762d2 ]) /\
    (Test_KWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ]) ==
     join [ 0xa8f9bc1612c68b3f, 0xf6e6f4fbe30e71e4,
            0x769c8b80a32cb895, 0x8cd5d17d6b254da1 ]) /\
    (Test_KWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF,
                       0x0001020304050607, 0x08090A0B0C0D0E0F ]) ==
     join [ 0x28c9f404c4b810f4, 0xcbccb35cfb87f826,
            0x3f5786e2d80ed326, 0xcbc7f0e71a99f43b,
            0xfb988b9b7a02dd21 ])
```

```
Test_KWAD :
   {a, n}
   (fin a, a >= 2, 4 >= a, n >= 2, 2^^54-1 >= n) =>
   [a*64] -> [(n+1)*64] -> (Bit, [n*64])
Test_KWAD k ct = (FAIL, join pt)
  where
    (FAIL, pt) = KWAD (\c -> AES::decrypt k c) (split ct)

property KWAD_RFC3394_Tests =
    (Test_KWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F ])
               (join [ 0x1fa68b0a8112b447, 0xaef34bd8fb5a7b82, 0x9d3e862371d2cfe5 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ])) /\
    (Test_KWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
               (join [ 0x96778b25ae6ca435, 0xf92b5b97c050aed2, 0x468ab8a17ad84e5d ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ])) /\
    (Test_KWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0x64e8c3f9ce0f5ba2, 0x63e9777905818a2a, 0x93c8191e7d6e8ae7 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF ])) /\
    (Test_KWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
               (join [ 0x031d33264e15d332, 0x68f24ec260743edc,
                       0xe1c6c7ddee725a93, 0x6ba814915c6762d2 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ])) /\
    (Test_KWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0xa8f9bc1612c68b3f, 0xf6e6f4fbe30e71e4,
                       0x769c8b80a32cb895, 0x8cd5d17d6b254da1 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ])) /\
    (Test_KWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E10 ])
               (join [ 0x28c9f404c4b810f4, 0xcbccb35cfb87f826,
                       0x3f5786e2d80ed326, 0xcbc7f0e71a99f43b,
                       0xfb988b9b7a02dd21 ]) ==
     (True, join [ 0x20ca3cba6f93747e, 0x456c666158ed83da,
                   0x3a6d614e94ba1ac5, 0xfe957c5963100091]))
```
 
# The end

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course Github page:
[https://github.com/weaversa/cryptol-course/issues](https://github.com/weaversa/cryptol-course/issues)

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
