# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```shell
cryptol> :m labs::KeyWrapping::KeyWrappingAnswers
```

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
module labs::KeyWrapping::KeyWrappingAnswers where
```

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

```
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
semiblocks. This is enough to build a simple type signature for `W`
which will contain the following components:

 * `n` -- A *type parameter* which controls the number of semiblocks
   in our inputs and outputs
 * `([128] -> [128])` -- The type of our *keyed* block cipher `CIPHk`
 * `[n][64]` -- The type of our string of input semiblocks
 * `[n][64]` -- The type of our transformed output

Putting these together we have our preliminary type signature:

```comment
W_prelim : 
  {n} 
  (fin n) => 
  ([128] -> [128]) -> [n][64] -> [n][64]
```

We haven't quite captured enough about the type of `W` -- for the
algorithm to operate correctly, and according to the standard, we will
have to make two more assumptions about `n`.

1. `n >= 3`: we can add this restriction to our type signature.
2. `n <= 2^^54`: this comes directly from the limits imposed by Table
   1 (Section 5.3.1, page 10). As an aside, if this constraint is left
   off, Cryptol's type checker will point out that `64 >= width
   (6*(n-1))` (which can be re-expressed as `6*(n-1) < 2^^64`). This
   constraint comes from the fact that the value `t` (which iterates
   over `[1..6*(n-1)]`) has to fit into a 64-bit word when passed to
   `WStep`. Of course, `2^^54` is less than `6 * (2^^54 - 1)` which is
   less than `2^^64`, so the tighter lower bound from Table 1 is
   acceptable.
 
```comment
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

**EXERCISE**: Study `Algorithm 1` and `Figure 2`. Implement `WStep`
  below assuming the appropriate input for `CIPHk` and assignments for
  `A'` and `Rs'` found in the body of the function. *Hint*: R2 mentioned
  in the spec is actually the first (`head`) semi-block from Rs.

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
*...we now return to our regularly scheduled program.*

We will use `foldl` along with our step function `WStep` to write a
definition for `W`.

**EXERCISE**: Complete the definition of `W` below by filling in the
  function skeleton provided.
 
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
command and sample output for `KWAETests`.

```shell
Cryptol> :check KWAETests
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
having Cryptol `:prove` the properties `WStep'Prop` and
`W'Prop`. Your output should look something like the following:

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
authenticates *as well as* computes the corresponding plaintext.

**EXERCISE**: Study `Algorithm 4` from the standard and complete the
  definition of `KWAD` below by filling in the function skeleton
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
by using the property `KWADTests` (this is defined later on
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
  recommend following the same steps from above and defining the helper
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
Triple-DES algorithm that's implemented in the `TDEA` module we
imported earlier. You can check the type of `TDEA` in the interpreter
via `:t TDEA::blockEncrypt` and `:t TDEA::blockDecrypt`. This has a
slightly different interface than the block cipher we used from the
`AES` module earlier. You can view how we use it here by looking at
`TestTKWAE` and `TestTKWAD`. It is worth taking a quick look through
the `TripleDES.cry` to learn a little bit about a particularly famous
NIST test vector.

You can test your work with the `TKWAETests` and `TKWADTests`
properties. Though, if you want to use them, you'll have to uncomment
them after finishing your work here. Good luck!

```
TWStep:
    {n}
    (fin n, n >= 3) =>
    ([64] -> [64]) -> [n][32] -> [32] -> [n][32]
TWStep CIPHk ([A] # Rs) t = [A'] # Rs'
  where
    [MSB, LSB] = split (CIPHk (A # head Rs))
    A'         = MSB ^ t
    Rs'        = tail Rs # [LSB]

TW :
    {n}
    (fin n, 3 <= n, n <= 2^^28) =>
    ([64] -> [64]) -> [n][32] -> [n][32]
TW CIPHk S = C
  where
    type s = 6*(n-1)
    ts     = [ 1 .. s ]
    C      = foldl (TWStep CIPHk) S ts

TKWAE :
    {n}
    (fin n, 2 <= n, n < 2^^28) =>
    ([64] -> [64]) -> [n][32] -> [n+1][32]
TKWAE CIPHk P = C
  where
    S = [ICV3] # P
    C = TW CIPHk S

TWStep' :
    {n}
    (fin n, n >= 3) =>
    ([64] -> [64]) -> [n][32] -> [32] -> [n][32]
TWStep' CIPHk' ([A] # Rs) t = [A'] # Rs'
  where
    [MSB, LSB] = split (CIPHk' ((A ^ t) # last Rs))
    A'         = MSB
    Rs'        = [LSB] # take Rs

TW' :
    {n}
    (fin n, 3 <= n, n <= 2^^28) =>
    ([64] -> [64]) -> [n][32] -> [n][32]
TW' CIPHk' C = S
  where
    type s = 6*(n-1)
    ts     = [ s, s-1 .. 1 ]
    S      = foldl (TWStep' CIPHk') C ts

TKWAD :
    {n}
    (fin n, 2 <= n, n < 2^^28) =>
    ([64] -> [64]) -> [n+1][32] -> (Bit, [n][32])
TKWAD CIPHk' C = (FAIL, P)
  where
    S = TW' CIPHk' C
    FAIL = head S != ICV3
    P = tail S
```


# A Formal Specification of `KWP-AE`

`KWP-AE` is the authenticated-encryption function and makes use of our
previously defined `W`. There are two new concepts to introduce with
this specification, so we'll break `KWP-AE` into two parts. The first
concept is that of **padding**.


## Concept 1: Padding

Many cryptographic specifications (especially hash functions) accept
some arbitrary number of bits as input but operate over some number of
words internally. Hence, it's common to see a bitvector **padded**
with zeros (or sometimes a constant and zeroes) to inflate the
bitvector until its size is a multiple of a word (usually 32 or
64 bits). For example, say we have a bitvector of `37` bits and we
want to pad it to fit into some number of 32-bit words. Well, the next
largest multiple of `32` is `64`, and `64 - 37` is `27`, so we'll need
to pad with `27` zeros. We can demonstrate this using Cryptol:

```
bits = 0b1001100101110011111010000001110011011 : [37]
bits_padded = bits # (0 : [27]) : [64]
```

However, what if we need to create a function that pads any size input
into a bitvector with a size that is a multiple of 32? In general, the
pad is what's needed to make something a multiple of a
word. Mathematically, this is identical to the **ceiling
modulus**. Backing up a second -- `a % b` gives the amount of `a`
remains after dividing by `b` (`%` is Cryptol's [remainder (or modulo)
operator](https://en.wikipedia.org/wiki/Modulo_operation)). Consider
we have `10` apples divided amongst `3` friends, after giving everyone
three apples, we'll have `10 % 3 = 1` apple remaining. What we desire
with padding isn't what's remaining, but rather the amount needed to
get to the next multiple, that is, how many **more** apples do we need
if we had another friend? --- To which the answer here is `2`. Or,
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
`32 + 32 + k*8 + k*8 %^ 64`, that is, 32 bits for `ICV2` plus 32 bits
for the number of octets of `P` plus `P` itself plus the shortage of
`P` as a multiple of `64` bits.

You may notice that the specification document gives the number of
octets to pad as

> ![](https://render.githubusercontent.com/render/math?math=8\cdot\lceil%20len(P)/64\rceil%20-len(P)/8)

where `len(P)` is the number of bits in P per the definition in
Section 4.1.

In general,
![](https://render.githubusercontent.com/render/math?math=b\cdot\lceil%20a/b\rceil%20-a=a%5C%25\hat{}b),
and since Cryptol supports the more concise shortage operator, we'll
use that instead.

**EXERCISE**: Study the first four lines of `Algorithm 5` from the
  standard and complete the definition of `KWPAEPad` below by filling
  in the function skeleton provided with appropriate logic.

```
KWPAEPad :
    {k, l}               // k is [len(P)/8], Algorithm 5
    ( 1 <= k, k < 2^^32  // Bounds on the number of octets of P, from Table 1
    , l == 32 + 32 + k*8 + k*8 %^ 64  // The type of S
    ) =>
    [k][8] -> [l]
KWPAEPad P = S
  where S = ICV2 # (`k : [32]) # (join P) # zero
```

## Concept 2: Oddly Typed `if-then-else` Statements

Sometimes, though not often, cryptographic algorithms will contain if
statements where the `then` and `else` branches return different
types. You were exposed to this a bit already in the [Salsa20
lab](../Salsa20/Salsa20.md). First off, this is always frustrating to
deal with in Cryptol, and we want you to know that we feel your pain
and we're sorry. Cryptol _can_ handle these types of situations, but
coming up with a solution requires experience with the type system
that is likely only learned through trial and error.

To dig into this a bit, let's consider the type of a generic
`if-then-else` statement

```sh
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

```
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

```sh
[error] at labs/KeyWrapping/KeyWrapping.md:663:1--666:14:
  Failed to validate user-specified signature.
    in the definition of 'f', at labs/KeyWrapping/KeyWrapping.md:663:1--663:2,
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
            at labs/KeyWrapping/KeyWrapping.md:666:13--666:14
        • a == 32
            arising from
            matching types
            at labs/KeyWrapping/KeyWrapping.md:664:13--664:14
[error] at labs/KeyWrapping/KeyWrapping.md:664:11--664:12:
  Type mismatch:
    Expected type: 48
    Inferred type: 32
[error] at labs/KeyWrapping/KeyWrapping.md:666:11--666:12:
  Type mismatch:
    Expected type: 48
    Inferred type: 64
```

This message tells us that `a`, the length of our input, has to
simultaneously be both `64` and `32` and (looking at the line numbers)
that these constraints come from the types of `g` and `h`.

In support of fixing the function, notice that since since `g` always
takes and returns 32-bit values, we have to shrink `x` from `a` bits
to `32` bits, and widen the result up to `48` bits. And, since `h`
always takes and returns 64-bit values, we have to widen `x` from `a`
bits to `64` bits, and shrink the result back down to `48` bits To
help us do this resizing work, we'll introduce `shrink` and `widen`
functions.

```
widen : {a, b} (fin a, fin b) => [b] -> [a + b]
widen a = 0 # a

shrink : {a, b} (fin a, fin b) => [a + b] -> [b]
shrink a = drop a
```

`widen` takes any sized input and prepends `0` or more `False`
bits. `shrink` takes any sized input and removes `0` or more bits from
the front. Using these two functions, we can fix our `f` from above:

```
f : {a} (32 <= a, a <= 64) => [a] -> [48]
f x = if `a <= 0x30 then
        widen (g (shrink x))
      else
        shrink (h (widen x))
```

And here we test that `f` correctly calls `g` and `h` (which increment
and decrement by 1, respectively).

```sh
labs::KeyWrapping::KeyWrapping> f (10 : [37])
11
labs::KeyWrapping::KeyWrapping> f (10 : [53])
9
```


## KWP-AE Top Level Function

With those two considerations firmly under our belt, we can now tackle
`KWP-AE`.

**EXERCISE**: Study `Algorithm 5` from the standard and complete the
  definition of `KWPAE` below by filling in the function skeleton
  provided with appropriate logic. Use the `KWPAEPad` function from
  above to create `S`. Use the `shrink` and `widen` functions to
  assist in resizing `S` and the function outputs on the `then` and
  `else` branches of line 5.

*Hint:* You'll notice that we needed to pull in the type variable `n`
and type constraints from `W` and relate `n` and `l` (the type of both
`S` and `C`). It may also be necessary to tell `W` that our type
variable `n` is the same type variable that it uses. This can be
achieved by calling `W` like so: ```W`{n=n}```.

```
KWPAE :
    {k, l, n}            // k is [len(P)/8], Algorithm 5
    ( 1 <= k, k < 2^^32  // Bounds on the number of octets of P, from Table 1
    , l == 32 + 32 + k*8 + k*8 %^ 64  // The type of S and C
    , 64*n == max 192 l               // Here we relate n and l
    ) =>
    ([128] -> [128]) -> [k][8] -> [l]
KWPAE CIPHk P = C
  where
    S = KWPAEPad P
    C = if (`k : [32]) <= 8 then
          widen (CIPHk (shrink S))
        else
          shrink (join (W`{n} CIPHk (split (widen S))))
```

Feel free to use the provided `KWPAETests` property to check your
work.


# A Formal Specification of `KWP-AD`

`KWP-AD` is the authenticated-decryption function and makes use of our
previously defined `W'`.

**EXERCISE**: Study `Algorithm 6` from the standard and complete the
  definition of `KWPAD` below by filling in the function skeleton
  provided with appropriate logic. We suggest splitting the algorithm
  into two again, so we're providing a skeleton for `KWPADUnpad`
  (which is roughly the inverse of `KWPAEPad`) and `KWPAD`. *Hint*:
  FAIL can be a Boolean expression, that is, it does not need to be an
  `if-then-else` statement.

```
KWPADUnpad :
    {k, l}               // k is [len(P)/8], Algorithm 5
    ( 1 <= k, k < 2^^32  // Bounds on the number of octets of P, from Table 1
    , l == 32 + 32 + k*8 + k*8 %^ 64  // The type of S
    ) =>
    [l] -> (Bit, [k][8])
KWPADUnpad S = (FAIL, split P)
  where
    MSB32 : [32]
    k     : [32]
    P     : [k*8]
    PAD   : [k*8 %^ 64]
    MSB32 # k # P # PAD = S
    FAIL = MSB32 != ICV2 \/
           k     != `k   \/
           PAD   != 0
```

```
KWPAD : 
    {k, l, n}            // k is [len(P)/8], Algorithm 5
    ( 1 <= k, k < 2^^32  // Bounds on the number of octets of P, from Table 1
    , l == 32 + 32 + k*8 + k*8 %^ 64  // The type of S and C
    , 64*n == max 192 l               // Here we relate n and l
    ) =>
    ([128] -> [128]) -> [l] -> (Bit, [k][8])
KWPAD CIPHk' C = (FAIL, P)
  where
    (FAIL, P) = KWPADUnpad S
    S = if (`k : [32]) <= 8 then
          widen (CIPHk' (shrink C))
        else
          shrink (join (W'`{n} CIPHk' (split (widen C))))
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

```sh
labs::KeyWrapping::KeyWrappingAnswers> :t KWPAE`{k = 2^^32 - 1}
KWPAE`{k = 2 ^^ 32 -
           1} : ([128] -> [128]) -> [4294967295][8] -> [34359738432]
```

Well, what's `34359738432`? Is it `2^^29` 64-bit words? Let's first
check how many 64-bit words it is. Here's one way:

```sh
labs::KeyWrapping::KeyWrappingAnswers> :t \(a : [34359738432]) -> groupBy`{64} a
(\(a : [34359738432]) -> groupBy`{64} a) : [34359738432] -> [536870913][64]
```

Great...now what's `536870913`? Is it `2^^29`?

```sh
labs::KeyWrapping::KeyWrappingAnswers> 2^^29 : Integer
536870912
```

Woh! Its not. `536870913` is `2^^29 + 1`. Let's double check this ---
here is a command that tests the `2^^29` upper bound from Table 1:

```sh
labs::KeyWrapping::KeyWrappingAnswers> :t KWPAE`{k = 2^^32 - 1, l = 64 * (2^^29)}

[error] at <interactive>:1:1--1:6:
  Unsolvable constraint: 34359738368 == 34359738432
```

And here is a command that tests the bound we just found, `2^^29 + 1`.

```sh
labs::KeyWrapping::KeyWrappingAnswers> :t KWPAE`{k = 2^^32 - 1, l = 64 * (2^^29 + 1)}
KWPAE`{k = 2 ^^ 32 - 1,
       l = 64 *
           (2 ^^ 29 +
            1)} : ([128] -> [128]) -> [4294967295][8] -> [34359738432]
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

```
TestKWAE :
   {a, n}
   (a >= 2, 4 >= a, n >= 2, 2^^54-1 >= n) =>
   [a*64] -> [n*64] -> [(n+1)*64]
TestKWAE k pt = join ct
  where
    ct = KWAE (\p -> AES::encrypt k p) (split pt)

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
     join [ 0x64e8c3f9ce0f5ba2, 0x63e9777905818a2a, 0x93c8191e7d6e8ae7 ]) /\
    (TestKWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ]) ==
     join [ 0x031d33264e15d332, 0x68f24ec260743edc,
            0xe1c6c7ddee725a93, 0x6ba814915c6762d2 ]) /\
    (TestKWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ]) ==
     join [ 0xa8f9bc1612c68b3f, 0xf6e6f4fbe30e71e4,
            0x769c8b80a32cb895, 0x8cd5d17d6b254da1 ]) /\
    (TestKWAE (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                       0x1011121314151617, 0x18191A1B1C1D1E1F ])
               (join [ 0x0011223344556677, 0x8899AABBCCDDEEFF,
                       0x0001020304050607, 0x08090A0B0C0D0E0F ]) ==
     join [ 0x28c9f404c4b810f4, 0xcbccb35cfb87f826,
            0x3f5786e2d80ed326, 0xcbc7f0e71a99f43b,
            0xfb988b9b7a02dd21 ])
```

```
TestKWAD :
   {a, n}
   (a >= 2, 4 >= a, n >= 2, 2^^54-1 >= n) =>
   [a*64] -> [(n+1)*64] -> (Bit, [n*64])
TestKWAD k ct = (FAIL, join pt)
  where
    (FAIL, pt) = KWAD (\c -> AES::decrypt k c) (split ct)

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
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F, 0x1011121314151617 ])
              (join [ 0x031d33264e15d332, 0x68f24ec260743edc,
                      0xe1c6c7ddee725a93, 0x6ba814915c6762d2 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ])) /\
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                      0x1011121314151617, 0x18191A1B1C1D1E1F ])
              (join [ 0xa8f9bc1612c68b3f, 0xf6e6f4fbe30e71e4,
                      0x769c8b80a32cb895, 0x8cd5d17d6b254da1 ]) ==
     (False, join [ 0x0011223344556677, 0x8899AABBCCDDEEFF, 0x0001020304050607 ])) /\
    (TestKWAD (join [ 0x0001020304050607, 0x08090A0B0C0D0E0F,
                      0x1011121314151617, 0x18191A1B1C1D1E10 ])
              (join [ 0x28c9f404c4b810f4, 0xcbccb35cfb87f826,
                      0x3f5786e2d80ed326, 0xcbc7f0e71a99f43b,
                      0xfb988b9b7a02dd21 ]) ==
     (True, join [ 0x20ca3cba6f93747e, 0x456c666158ed83da,
                   0x3a6d614e94ba1ac5, 0xfe957c5963100091]))
```

```
TestTKWAE :
   {n}
   (n >= 2, 2^^28-1 >= n) =>
   [192] -> [n*32] -> [(n+1)*32]
TestTKWAE (k0#k1#k2) pt = join ct
  where
    ct = TKWAE (\p -> TDEA::blockEncrypt (k0, k1, k2, p)) (split pt)

property TKWAETests =
    (TestTKWAE 0x12b84c663120c196f8fc17428bc86a110d92cc7c4d3cb695
               0xef7da3da918d0679
            == 0x7a72bbca3aa323aa1ac231ba) /\
    (TestTKWAE 0xee9e19d319e4669d46fafc47f266277788017de0e38a4eaf
               0xd96075b0ee4525fe0bbe2abbd49d4d68e15a37c2bb11bd79a491e13ed3d32a48aef0d1fbcd9a65c7e0bf3cbba92366cb23b4b0876d509539df8f6dd8edb5eb9d8487ec42497b579b8a660a95455063bb7703ea6d57717c7d3625c86e3162c78b13068605e9e1c6ebed9d8f8b2b72784de432bb1ae596d4210538d4ca8577878ec42fa25694bd12a0a27423c62dd0e35a2781b8fecab7066871f4f4d506d41fceb63b669558c1df622259eaefd33bbd6af871798ca98d469a6c5a8bef284318b9692263b7c84c1d352bea16c3ee40b86061aa99d7e60013d618f579111f1e03685ede51b9fe7cde04416718db5f7d322a6f8547c354c8ee9a8f176de5f090e08e
            == 0x61a9e9ec40462f6915179fef341b41cfcebe70f4539cc8babb6d6083905ead8f62f1e1cebf8045a7930812acd93227c09be10758dccd1afdb997931adeeb5e39749aaec607bc8c0346c0d864e65f61a4d7de4c3aea6cbcd9ff5c7664e705b441d8bce4b44ffbfb796a81e491eab7b817382ff87c8f6ce855ab80e234432ed7cf3d7bfddc15406d4b07fd0455c23669306634fccefb0cef69f693ec6c075912ec23342116d28453f7e1d2df05585d9a03e6e8ed3b8e2e0a95394b19779e26f845245c0806f7f619c618889caa9167f8908c0325923a1118cdd2c0a48b8b2b224c5faec7beb7d6a7d54e4212783f01661fb570ab1aa2589a6e922c9c3accc308b8088b601e)
```

```
TestTKWAD :
   {n}
   (n >= 2, 2^^28-1 >= n) =>
   [192] -> [(n+1)*32] -> (Bit, [n*32])
TestTKWAD (k0#k1#k2) pt = (FAIL, join ct)
  where
    (FAIL, ct) = TKWAD (\p -> TDEA::blockDecrypt (k0, k1, k2, p)) (split pt)

property TKWADTests =
    (TestTKWAD 0xe273cd9d7210a973b4113c5772474938d353b54e265dd944
               0x10a38310b604b48f94357d67
            == (False, 0x2ffd56320f1dff99)) /\
    ((TestTKWAD 0x8476d056582e322d93ab9919086798ba48d03eddf77803e5
               0x2c9bf0131cf486402422b8ef).0
            == True) /\
    (TestTKWAD 0xee9e19d319e4669d46fafc47f266277788017de0e38a4eaf
               0x61a9e9ec40462f6915179fef341b41cfcebe70f4539cc8babb6d6083905ead8f62f1e1cebf8045a7930812acd93227c09be10758dccd1afdb997931adeeb5e39749aaec607bc8c0346c0d864e65f61a4d7de4c3aea6cbcd9ff5c7664e705b441d8bce4b44ffbfb796a81e491eab7b817382ff87c8f6ce855ab80e234432ed7cf3d7bfddc15406d4b07fd0455c23669306634fccefb0cef69f693ec6c075912ec23342116d28453f7e1d2df05585d9a03e6e8ed3b8e2e0a95394b19779e26f845245c0806f7f619c618889caa9167f8908c0325923a1118cdd2c0a48b8b2b224c5faec7beb7d6a7d54e4212783f01661fb570ab1aa2589a6e922c9c3accc308b8088b601e
            == (False, 0xd96075b0ee4525fe0bbe2abbd49d4d68e15a37c2bb11bd79a491e13ed3d32a48aef0d1fbcd9a65c7e0bf3cbba92366cb23b4b0876d509539df8f6dd8edb5eb9d8487ec42497b579b8a660a95455063bb7703ea6d57717c7d3625c86e3162c78b13068605e9e1c6ebed9d8f8b2b72784de432bb1ae596d4210538d4ca8577878ec42fa25694bd12a0a27423c62dd0e35a2781b8fecab7066871f4f4d506d41fceb63b669558c1df622259eaefd33bbd6af871798ca98d469a6c5a8bef284318b9692263b7c84c1d352bea16c3ee40b86061aa99d7e60013d618f579111f1e03685ede51b9fe7cde04416718db5f7d322a6f8547c354c8ee9a8f176de5f090e08e))
```

```
TestKWPAE :
   {a, k, l, n}
   ( a >= 2, 4 >= a
   , 1 <= k, k < 2^^32
   , l == 32 + 32 + k*8 + k*8 %^ 64
   , 64*n == max 192 l
   ) =>
   [a*64] -> [k*8] -> [l]
TestKWPAE k pt = ct
  where
    ct = KWPAE`{k, l, n} (\p -> AES::encrypt k p) (split pt)

property KWPAETests =
    (TestKWPAE 0x6decf10a1caf8e3b80c7a4be8c9c84e8
               0x49
            == 0x01a7d657fc4a5b216f261cca4d052c2b) /\
    (TestKWPAE 0x76f37eac56a4830945102ea2548daf4a
               0x236fe4754222b8d365ff2c86e5d5e7a37704361b9dde550d6f7d9efc19b0ff2171db8b7979e404d9a80fdf57c122108d83d09b9479d0c6c210587c0c6e5efc8356e50dcd4f87e25518ee8d3fe72d4b357b4329c009cf1a838d2e022c97dfe9cff9b55df5262bbab5f48cc5f7a51e59e1ab4ab08463dbb7b43c3fe65372d2453bba96c245803df76f104246e01a5bc31053827c784fee50d0111423704985f1db8dad03aa9105e76cc509557055d20f34d78c7245d8003521cae7a74d5a9de9db947f3669f41bc0581c6c27fc3aa6b90ede76a5449a5359eca187d418ee4949ffc415543fab451eb71b3b92e43450a943683bdb5bb16b06b12ef356e061206078c8c4d69b0ce945ee8cd6d5008400c4460f74e3a1f26d9e89034a190cabf98b84c0cc35d4540ce41a5abd628179ec2d087b82f4d3582cfd432a0cfcbb0684c6512ff4900979e3f28f3ff4218a101817883a2de60f35262b6f82173242d06a01cff339abe314d8a7c1ed80e721947420a5e5e5adf71d2b39abb0e31b2bfb1f4ef9bc9cb60f2930b48a654612f49a2d52537cd966eb9ac8f23d32dc10167c2ba21e8d3ffcc91f0e2aef73f7583a30c2d1417a4e94f30fb1f4626e95a63943a68fc615e122b39aeb6be5b8b55580c78e6449ad09ba86d5dde97c4ab9853fa0f98edc675d23e192cbc96f0d678c7e3f6de0292a26d939aa97ae98b44c3b16f53c55f1
            == 0xef9c70fdbdab68d16246bbf93088cae29efa85cb4c5b00b4e6743e9cbd7d714229ef9f072d42f441ac66eb2ec1e71bec35c2555b2ea961403ac9d0fa8b3a06b3a0f897afea0e7317caf82033e2de04bf6cdb5269a5d3cbeecc2e89148d75dfadc31430c1b66cc285347eff43e8c91c6b245fcbb65cc33b599c623b1823c79af39b0bb98440595157a90fe045cbcd6fd2c240869eb01d867d0eea7cc08deb081ce488e767c1fb441020db9416e0887e323dc7cb12cf26a5c6be4a9abbc99c207a02836f11f32441c5cd8c1e298a0deb142f2c6eecead290fbe67edc1de748dc56317cb7084781f29f44df282171062a4723d2a9d189ddb7fb048a56a48e607cfccacc7b3f351acc9b6d97607412a8e3d0d58ae40d6e8c9989284cc42712113411f5c7d0b19e7184418d33c4f70742316a49566aec1b7de8d0514dca97111d571f40828b3ab097a708311be10847db08bac4e69949d89e8d02369ec13cb06a5d9ccdfd2afdfe291e5d56b5149ba7e55236925eaf322fbc50e942289cbec6b66db924aa0107a69002208e282b4314ad20d60b67c80a4632a630e014867a23fb96f13aa5da5ddb3480e269d7b607ccc6e832bc1f83bd6e4553118def549693afc3b9b60038d65956769cb74bb09892348fcf1daf5563e6592e11b48d6b34914b1f6ccaf26b46f2717e284ffcc27e8c574f8b8b9690bb66c25d7c5281ac36b75e2ab3a96b253c0008cbdf) /\
    (TestKWPAE 0x9ca11078baebc1597a68ce2fe3fc79a201626575252b8860
               0x76
            == 0x866bc0ae30e290bb20a0dab31a6e7165) /\
    (TestKWPAE 0x8b190ea5cab54000d037fda56859521f0af545bdc80b8ac3
               0x0cdb4f843aa332077bf9b52340343085191e0e72e4f6a520ff7e561406902e188ae09fb451a087d9862279a96c4ee8b599a4c4c15e450cecf8df6f0ae879fd5c359bba26dcb308dd3e700e0511f01a72b7e525ddceaf06b1b311c8f22da10fec9a9555f78e5ebcb6015175cc6e6ed9f79d3f5e637bbdba2642c0c63ff66b3f711c1d3e9e39dd588209d1131eabb910f2cd4561ff0611397a74837d5009aecbb6901d8228039627f42c3372bbd7a57797db563c8ae5a1809c6d71b7a36875529544da375ab2cd48ea8931b2a905f84c3a4513c6ddb05a37ed7a9efed062c75f25435f4ade509864555408798be3886dde1fc3c3897622e1d63821b4238348cf331aca9379608f9f5fdf907dd72f25c7d1e8671b5db4217315c23d72a7bab12e1156d18d4b3d3263aef1e1e11603de7c958dfc60f989f38dd994c064d5128677740f3ab65ca8d42f6c40b779b669a27ef3ce91baf040b0674a34cc6fb23a75c944ce63bf359aea107eac0823b25b94b640d24c31fef796f3485ca316e91a2175eecbbb6be3dc947b8eeb2d502c3442e995046d959ea78206f18aeeafd844dac654aa0d92e03756ade850537d58bba15ca760fee9156fcac540cf85de7cb7ecb47a7bfb17bf8f98d39cee83e0caeebde4b8985cd0035419c727599d9e07ea807eaba08eb21a5644c7ced157d07d2b2766495788267b77eb2c71af2c49406529d5a3
            == 0x05a9711e3248192d0513f859cac33f5cd036a30ab1390d9e50849ea4ccab2d015eb352174440bff2bf6ca06c27267b05d867ae907be30a9d4df51c2af192ae17101edfe9d776e22341289daa30b0539ee2066e05691f18ebed41b2fcdfcec22f6c0108565e24228096a7bd98982fb45761c8bb9b7155303beab7488f1341e87f9549fecd41a71e3ca7c93f8374275852da1a8d1c5ade015507a2044353e9a598280779dec798d047c22284e0968367ef9decc66cd8ebc8a340adb4c679602c869c02a53090feb2353aeca6507a6aee7a5ddb409476ea8255c722a4d151a97253180ffafeee6fad2d40da285d188d68f4d1c1ec9d925f976d43143bc35caa306fc7178b2ddc8c9342c31c385fff69f600dac1b3ad6c590655047b6371dd9cff8eadd480b0305e03b35ad90a3a75671abf2ad73d5b75851e4413f608b1e6df083540042c67e7c54c6760971ad022975c90f42454f38dea0bfc7d8b01691a74f0f29f9e0133a1484682c96ab7c0b4d347394d69fff364327eaa7af8d71e72ca8176cefff901fcef6f960a63b0b6304f59ee9fb62f628b5e6d3783068c2f6203982188de009373eb0c8422a5f0d4201e31876414ca84a398d68bdd9cda50ac663a3e05abc8698113d6ce6fe3538e785e96ea148732018812b1f62e80848a88a65f7dca04aab3f9f9b8968d0ae8f299974af036676eb21617f143bda55dbe92921001f17ccf655de53586) /\
    (TestKWPAE 0x95da2700ca6fd9a52554ee2a8df1386f5b94a1a60ed8a4aef60a8d61ab5f225a
               0xd1
            == 0x06ba7ae6f3248cfdcf267507fa001bc4) /\
    (TestKWPAE 0xa8571d0257d79010ab20d01858f4763c03e628f912786119850614d9023950b3
               0xed9061eac56e8fc571134219e5f713a7a57cb11760e2e7848ab38102a7ce4e1e2a4e18e4da4d13978c6d41f8e05969a140c40929fd0f57f073d83c15f56a1f5a5db0e9b0da57a763e85ae365e3a24c79ec6b9ce90e12bd9fe080c357a469b2fcbe2e897a277764c97f4af160acac71c9fe6110c19ead71d2a7ce9624a0d1f9639867559e93bffb81fdedfe30a408bc4c69b56a17aced776c1fcb0e956351bcc5c2102ad7a4b3274f3a9e3ab9a9885830c30942cd210afc260971e879caf74aef71ae6d083a0362b047dad38d91773a555da7dc6d6377b8cc8c8c158ab708a02479be5362854f45c7f119a4da9adef76ef5bc42a7be85ee8cfab244673b649c9bfc52faee6e46af2322a65c1a07cb40c6f78f36d5519c8d15edcf5bab4ae00a8106ef7389029d796367c49c1555680ef910e0844fe7b0b7c47188ce0b64ad6f3b57a6507f825b8b05d95b30c407f127ac73d9e25cf70992586e037edd1edd897f863c8a052543b68eccaa2eb4199b045a27310effc0168591a4b32484e8541c79dc8b61e749828f03a3313edfd75d0336aeb2c64d5900df9ba5afc9c2302385bfa032f0f2f775762dab19df0dc464c67ff9537636b9ea0fd58ff210376e735e4d5c54129fbe55694f170476cf5a1511e11ab71d5be8590ce6f073b7daed59d3d9fd36ad804e2b3f2d5f4207d65f014cb0351a4c774bfaae7c0777b9e0f5e7eca6
            == 0x0e522e2dc48dcd4833226ca86439a2cd6188315794a41098fe3e7bc6e442212fc216d5bd682a6829b61fbb6d466b4cd82da87a40ade8833b4782b23ed869e2d47275a12fb21a4ca980458e64e4ce7e95c228b570f536179f4df2842302f0efaee7be28c7ca7f7edab902cb11a0325eaf2abf45fffff78f72a5a1061ea579592407866edb9863a2d05445bf4f21778ad620164c289d8fcc31e846bea4dc251c9baf7b7211296a9229148447dbffafeb780bb18a2436689cb897f791af171fac294f1b289dba66b233e42022548cb904222a09b742fd6a9a5c2e864124dea1be9877661cfc7f28810a4c2448f00c7d07a7ea9daae6a9a18569e68620df9f35a66b585c00d411912f6858ec8f70931ecc692c38b72ec096850fa5d7a77c826d9d053ee859ebea486b8d1802a4879b3237a36f773c13806a2449faefa9bf30dc5176b758d153cb2a0373ff795a58da303a48cd3531423926160be19dce1b1a9dc76739e01baa86e489cb8d378ad40e0a1ebeb4a0ffa246762915d92ba1e78449df45d0b99a78b74d64da3cf03fed733f36fe5a23d48ceab01446aa67433b8a2b15610ee27a5bb81247d2172c4e555094e957efac906e5fb502a46e7097fb42251885782f7e0cb19ee0e4028f659447647ebdd2d0b59e026874e44b62d8dd862fda89095fa170af25b156d7687152cc6cd50aebc143a781b9802484eb068ae284aa9db54e8b5cf4303a4c)

TestKWPAD :
   {a, k, l, n}
   ( a >= 2, 4 >= a
   , 1 <= k, k < 2^^32
   , l == 32 + 32 + k*8 + k*8 %^ 64
   , 64*n == max 192 l
   ) =>
   [a*64] -> [l] -> (Bit, [k*8])
TestKWPAD k ct = (FAIL, join pt)
  where
    (FAIL, pt) = KWPAD`{k, l, n} (\c -> AES::decrypt k c) ct

property KWPADTests =
    (TestKWPAD 0x49319c331231cd6bf74c2f70b07fcc5c
               0x9c211f32f8b341f32b052fed5f31a387
            == (False, 0xe4)) /\
    ((TestKWPAD`{k=1} 0x30be7ff51227f0eef786cb7be2482510
                      0x7f61a0a8b2fe7803f2947d233ec3a255).0
            == True) /\
    (TestKWPAD 0x58e7c85b60c7675002bd66e290d20cc694279f0bfc766840
               0xf2edd87dabb4a6ae568662f20fcc4770
            == (False, 0x76)) /\
    ((TestKWPAD`{k=1} 0x94c8dae772a43b5e00468e0947699b239dfe30ab5f90e2f6
                      0x239c6bceee3583fe7825011e02f01cc0).0
            == True) /\
    (TestKWPAD 0xf89732cee8226735ee03020093d02ba1d1f658bd192532f89f14c4706752b50f
               0x784b3288625af3fd5b675c0bf72e08be1dded4cfeb1bfa007dde6cce875010e265ec6b0af91d6c84
            == (False, 0x9a5c3c6fbca7007e1ec9fa62f50c46af2186f5ab3b5e936ef9845ecd362a6d)) /\
    ((TestKWPAD`{k=27} 0x9c2f01de02319ddedcd6193769a6e134ed2ef4b2e36b0edc1719fbd3bc3850b4
                       0xe3f3b2ed794fb604a162911a7d8e8b44d7b9ba65834ed9cadf410e27fdbd11934ee1490205f4c2b6).0
            == True)
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
