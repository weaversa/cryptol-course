# Introduction

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in associated files.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * the `:prove` command,
  * writing functions and properties,
  * sequence comprehensions,
  * functions with curried parameters, and
  * parameterized modules.

## Skills You'll Learn

By the end of this lab you will have gained experience with Cryptol's
new module system to define common *interfaces* and *functors* to
generate related implementations and properties, enabling you to
create reusable families of cryptographic algorithms and modes, and 
*nested modules* to combine these into one or more Cryptol modules.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::NewModuleSystem::NewModuleSystem
Loading module Cryptol
Loading interface module `parameter` interface of labs::SimonSpeck::Simon::Simon
Loading module labs::SimonSpeck::Simon::Simon
Loading mdoule labs::NewModuleSystem::NewModuleSystem
```

We start by defining a new module for this lab:

```cryptol
module labs::NewModuleSystem::NewModuleSystem where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.  
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Cryptol's New Module System

In a previous lab on parameterized modules, you learned how to specify
[*parameterized modules*](../SimonSpeck.md) and use these to generate
families of related
[Simon](https://en.wikipedia.org/wiki/Simon_(cipher)) and
[Speck](https://en.wikipedia.org/wiki/Speck_(cipher)) modules from
common parameters.  Now suppose we wish to define
[block cipher modes of operation](https://en.wikipedia.org/wiki/Block_cipher_modes_of_operation)
such as
[Output Feedback (OFB)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Output_feedback_(OFB))
or [authenticated encryption modes](https://en.wikipedia.org/wiki/Authenticated_encryption)
such as
[Galois Counter Mode (GCM)](https://en.wikipedia.org/wiki/Galois/Counter_Mode).
Using the techniques covered so far, we would have to specify similar
`parameter` blocks for each of the different modes, define similar
`property`s for them, and generally repeat ourselves a lot, which is
[WET](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself#WET) (bad).

In this module, we will leverage Cryptol 3.0's powerful
[new module system](https://galoisinc.github.io/cryptol/master/Modules.html)
to define:
  * a reusable
    [`interface`](https://galoisinc.github.io/cryptol/master/Modules.html#interface-modules)
    for block ciphers
  * multiple block cipher *implementations* of this interface
  * multiple block cipher modes as
    [*functors*](https://galoisinc.github.io/cryptol/master/Modules.html#instantiating-a-parameterized-module)
    to *instantiate* with any of these block cipher implementations
  * functors to generate common properties and algorithm/mode-specific
    test vectors from implementations of this same interface
  * Cryptol batch files to check test vectors and
    SAW proof scripts to verify more complex properties

This is a tall order, but powerful new tools and features will help.
Let's get started...

# Nested Modules (`submodule`)

First, an administrative item: Cryptol's new module system is complex!
For consistency with other modules in our Cryptol course, we present
this as a single
[*nested module*](https://galoisinc.github.io/cryptol/master/Modules.html#nested-modules).
This is a new feature that enables us to present multiple
`submodule`s -- e.g. multiple instances of a common parameterized
module -- in a single file, which was not previously an option for
examples like the earlier
[**Parameterized Modules**](../SimonSpeck/SimonSpeck.md) lab.

# Symmetric Block Ciphers and Modes of Operation

Following along from [1], a *symmetric block cipher* comprises a pair
of algorithms:
  * *encryption* (a "forward cipher operation") and
  * *decryption* (a "reverse cipher operation")

Each accepts...
  * a **key** (of arbitrary type; usually a bit string)
  * a **block** (a bit string)

...and produces the corresponding ciphertext or plaintext.

A **block cipher mode of operation** applies an underlying symmetric
block cipher over some number of blocks (or sub-blocks).

# Interfaces

Using prior techniques for parameterized modules, we might start each 
mode's (sub)module with a parameter block (commented out here because
that's not how we'll finish...):

(Here, we use the terms `encipher` and `decipher` to distinguish block
cipher operations from those for modes, in keeping with [1].)

```cryptol
/*
parameter
  /** arbitrary key type */
  type K : *
  
  /** block size (in bits) */
  type b : #
  
  /** forward cipher operator */
  encipher : K -> [b] -> [b]
  
  /** reverse cipher operator */
  decipher : K -> [b] -> [b]
*/
```

But this approach already repeats itself, and we'd have to start each
(sub)module with the `parameter`s in this block (along with any others
unique to the mode).  It would be better to reuse the common block
cipher parameters, and we can do so by defining an **interface** with
reusable **type aliases**:

```cryptol
/** common parameters for symmetric block ciphers */
interface submodule I_BlockCipher where
  /** arbitrary key type */
  type Key : *
  
  /** block size (in bits) */
  type b : #
  
  /** type alias for a block, a bit string of width `b` */
  type Block = [b]
  
  /** common type for `encipher` and `decipher` */
  type Op = Key -> Block -> Block
  
  /** forward cipher operator */
  encipher : Op
  
  /** reverse cipher operator */
  decipher : Op
```

# Functors

Each mode's (sub)module can then `import` this `interface` to serve the
same purpose as previously for `parameter` blocks: to indicate that the
module needs these parameters:

```cryptol
/* (Not quite ready to define these modes yet...

submodule ECB where
  import interface submodule I_BlockCipher
  // Now `type K`, `type b`, `encipher` and `decipher` are in scope
  // But unfortunately, `type Block` and `type Op` are not...
*/
```

This is an example of a
[**functor**](https://galoisinc.github.io/cryptol/master/Modules.html#importing-an-interface-module)
-- the modern, more versatile update to parameterized modules.  Later,
we will see that such functors can accept multiple interfaces, and
define additional type constraints on their implementations.

# Instantiation

As noted above, interfaces do not (yet) export their type aliases, but
it would be nice for users to have access to these.  We'll quickly do
this with a functor that imports the same interface and exports the
(copied) type aliases.  This also demonstrates an **interface alias**
to distinguish the interface's aliases (which would otherwise conflict
despite not being accessible) from the functor's:

```cryptol
submodule F_BlockCipher_Aliases where
  // aliased interface import to avoid conflict
  import interface submodule I_BlockCipher as I

  /** type alias for a block, a bit string of width `b` */
  type Block = [I::b]
  
  /** common type for `encipher` and `decipher` */
  type Op = I::Key -> Block -> Block
```

Then a `mode` can...

```cryptol
/*
  import interface submodule I_BlockCipher as I
  import submodule F_BlockCipher_Aliases { interface I } (Block, Op)
*/
```

...and reuse these aliases in its own definitions.

Of course, parameterizable block cipher modes need block ciphers!  We
recently implemeented the Simon and Speck block ciphers, which are
themselves parameterized.  We just need to adapt these to our fancy
new interface...

## Explicit Instantiation

We could do so explicitly...

```cryptol
/** Simon 32/64 implementation of `labs::SimonSpeck::Simon::Simon`'s parameters */
submodule P_Simon_32_64' where
  type n = 16
  type m = 4
  type T = 32
  type j = 0

/** Simon 32/64 instance */
submodule Simon_32_64' = labs::SimonSpeck::Simon::Simon { submodule P_Simon_32_64' }

/** explicit implementation of `submodule I_BlockCipher` with Simon 32/64 */
submodule P_Simon_32_64_BlockCipher' where
  import submodule Simon_32_64' as S
  type Key = [S::keySize]
  type b = S::blockSize
  encipher = S::encrypt
  decipher = S::decrypt
```

This approach makes sense if we expect (us or our users) to refer to
these submodules later.

`labs::SimonSpeck::Simon::Simon` can be instantiated because its `parameter`
block is treated by the new module system as an
[**anonymous interface**](https://galoisinc.github.io/cryptol/master/Modules.html#anonymous-interface-modules).

## Anonymous Instantiation

If we don't intend to reuse these underlying implementations, we can also
implement our interface with an
[**anonymous instantiation**](https://galoisinc.github.io/cryptol/master/Modules.html#anonymous-instantiation-arguments):

```cryptol
/** anonymous implementation of `submodule I_BlockCipher` with Simon 32/64 */
submodule Simon_32_64_BlockCipher'' where
  // ...by importing an anonymous instance of `labs::SimonSpeck::Simon::Simon` into a namespace `S`
  import labs::SimonSpeck::Simon::Simon as S where
    type n = 16
    type m = 4
    type T = 32
    type j = 0

  type Key = [S::keySize]
  type b = S::blockSize
  encrypt = S::encrypt
  decrypt = S::decrypt
```

**Exercise**: Implement (define submodules that define types and values
for) `I_BlockCipher` for other Simon and Speck parameter sets, from
solutions (yours or ours) for
[`labs::SimonSpeck::SimonSpeck`](../SimonSpeck/SimonSpeck.md).  Later,
we will define multiple block cipher modes for each of these block
ciphers and parameter sets...

**Hint**: This will be less tedious if you define an `interface` and
functor with which to "adapt" prior solutions as implementations of
`I_BlockCipher`.  Instances would look something like:

`submodule P_Speck_64_128_BlockCipher = submodule F_Speck_BlockCipher { labs::SimonSpeck::SpeckAnswers::speck_64_128 }`

Start with explicitly named submodules. These can always be
"anonymized" later if it is obvious they won't be reused.

```cryptol
/** interface to collect relevant definitions from Simon instance */
interface submodule I_Simon_Inst where
  type keySize : #
  type blockSize : #

  type Key = [keySize]
  type Block = [blockSize]
  type Op = Key -> Block -> Block

  encrypt : Op
  decrypt : Op

// generalization of `Simon_32_64_BlockCipher` above
/** generate `I_BlockCipher` implementation from Simon instance */
submodule F_SimonSpeck_BlockCipher where
  import interface submodule I_Simon_Inst as S

  // export definitions for `I_BlockCipher` parameters
  // type Key = [S::keySize]
  // type b =  // Define and uncomment.

  encipher = S::encrypt
  decipher = undefined  // Define and uncomment.

/** `I_BlockCipher` implementation for Simon 32/64 */
submodule P_Simon_32_64_BlockCipher = submodule F_SimonSpeck_BlockCipher { labs::SimonSpeck::Simon::simon_32_64 }

// /** `I_BlockCipher` implementation for Simon 48/72 */
// submodule P_Simon_48_72_BlockCipher = // Define and uncomment.

// Repeat for remaining Simon and Speck parameter sets.
```

## Adapting other block ciphers

In the last exercise, you adapted your own prior work to fit out new
`I_BlockCipher` interface.  We can also adapt other simple block
ciphers, such as DES in the
[Cryptographic Proofs](../CryptoProofs/CryptoProofs.md) lab.

```cryptol
import specs::Primitive::Symmetric::Cipher::Block::DES (DES)
```

**Exercise**: Adapt `DES` with a submodule that provides all parameters
required by `interface submodule I_BlockCipher`:

```cryptol
submodule P_DES_BlockCipher where
  // type Key = ? // Define and uncomment.
  type b = 64
  
  encipher = DES.encrypt
  decipher = undefined  // Replace with your definition.
```

# Block Cipher Modes of Operation: ECB, CBC, ...

Having implemented various block ciphers, we can now apply them toward
**block cipher modes of operation** such as:
  - [Electronic Codebook (ECB)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Electronic_codebook_(ECB))
  - [Cipher Block Chaining (CBC)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher_block_chaining_(CBC))
  - [Cipher Feedback (CFB)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher_feedback_(CFB))
  - ...

Each cipher mode repeatedly applies a block cipher to multiple blocks
in a message -- often with an **initialization vector** and
intermediate transformations, and possibly after **padding** the
message to evenly divide it into blocks -- to produce a ciphertext
message with the same number of blocks.  (Authenticated encryption
generates a **signature** alongside the ciphertext.)

## Electronic Codebook (ECB)

ECB is a simple block cipher mode that just enciphers each block, with
no intermediate transformations to **diffuse** the message, a weakness
that disqualifies it for use in security-critical settings.  But just
as a pedagogical exercise, we can define ECB in Cryptol...

**Exercise**: In the specification of ECB mode below, define `decrypt`
so that using the same shared key on an encrypted ciphertext message
produces the original plaintext.  Do not include an argument for the
message in your definition.

```cryptol
/** Disclaimer: Weak block cipher mode; do not use in production. */
submodule ECB where
  // request a block cipher
  import interface submodule I_BlockCipher

  // derive the type aliases for that block cipher
  import submodule F_BlockCipher_Aliases { interface I_BlockCipher } (Block)

  /** Encrypt a multi-block message using ECB mode */
  encrypt : {n} Key -> [n]Block -> [n]Block
  encrypt key = map (encipher key)

  /** Decrypt a multi-block message using ECB mode */
  decrypt : {n} Key -> [n]Block -> [n]Block
  decrypt key = undefined  // Replace with your definition.
```

Again, please do not ever use this weak cipher mode in the real world.

**Exercise**: Define submodules for each Simon and Speck parameter set
in ECB mode.

```cryptol
/** Simon 32/64 in ECB mode */
submodule ECB_Simon_32_64 = submodule ECB { submodule P_Simon_32_64_BlockCipher }

/** Simon 48/72 in ECB mode */
// submodule ECB_Simon_48_72 = ... // Replace with your definition and uncomment.

// Repeat for each Simon and Speck parameter set.
```

**Exercise**: Likewise, define a submodule that implements DES (a weak
block cipher) in ECB (a weak cipher mode).  Add a docstring with a
prominent warning not to use it in production.

```cryptol
// /** Add a docstring */
// submodule ECB_DES = submodule ... { submodule P_DES_BlockCipher }  // Replace with your definition and uncomment.
```

Your solution should pass the following test vectors [3]:

```cryptol
submodule ECB_DES_Test where
  import submodule ECB_DES as ECB_DES

  e (key, pt, ct) = ECB_DES::encrypt key pt == ct
  d (key, pt, ct) = ECB_DES::decrypt key ct == pt

  // from FIPS 81 (withdrawn, of course)
  v_fips_81 = (0x0123456789abcdef, [0x4e6f772069732074, 0x68652074696d6520, 0x666f7220616c6c20], [0x3fa40e8a984d4815, 0x6a271787ab8883f9, 0x893d51ec4b563b53])
  property e_fips_81 = e v_fips_81
  property d_fips_81 = d v_fips_81
  
  // another test vector based on `https://opensource.apple.com/source/OpenSSL/OpenSSL-23/openssl/test/evptests.txt.auto.html`
  v35 = (0x1111111111111111, [0x1111111111111111, 0x0123456789ABCDEF], [0xF40379AB9E0EC533, 0x8A5AE1F81AB8F2DD])
  property e35 = e v35
  property d35 = d v35
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :check ECB_DES_Test::e_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :check ECB_DES_Test::d_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust ECB_DES_Test::e35
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust ECB_DES_Test::d35
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust ECB_DES_Test::e53
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust ECB_DES_Test::d53
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Cipher Block Chaining (CBC)

To better diffuse message blocks, CBC introduces an *initialization
vector* and applies the binary exclusive-or operator (`^` in Cryptol)
to each block of plaintext with the previous ciphertext (the
initialization vector for the first block) before applying the block
cipher with a given key.  That is:

`c0 = encipher key (p0 ^ iv)`
`c1 = encipher key (p1 ^ c0)`
`c2 = encipher key (p2 ^ c1)`
`...`
`cn = encipher key (pn ^ ...)`

**Exercise**: Define a functor to generate a CBC mode for a given block
cipher.

```cryptol
submodule CBC where
  // request a block cipher
  import interface submodule I_BlockCipher

  // derive the type aliases for that block cipher
  import submodule F_BlockCipher_Aliases { interface I_BlockCipher } (Block)

  /** Encrypt a multi-block message using CBC mode */
  encrypt : {n} Key -> Block -> [n]Block -> [n]Block
  encrypt key iv pt = undefined  // Replace with your definition.

  /** Decrypt a multi-block message using CBC mode */
  decrypt : {n} Key -> Block -> [n]Block -> [n]Block
  decrypt key iv ct = undefined  // Replace with your definition.
```

**Hint**: The above definition can be directly translated to a Cryptol
[generating function](https://galoisinc.github.io/cryptol/master/BasicTypes.html#sequences).
(See `generate` and the examples following it.)  So for `encrypt`, you
can say `encrypt key iv pt = ct where ct@i = ...` and then fill in the
rest.  Remember to account for the initialization vector and the first
ciphertext block `ct@0`.

**Note**: Though we suggest generator expressions here, it is also
possible to use various higher-order functions such as `foldl` and
`zipWith`.  For now, this is mostly a matter of style.  However, when
it comes time to verify that our submodules meet important properties
(most notably, that using the same key and initialization vector
to decrypt ciphertext recovers the original plaintext), this choice
will greatly affect future verification efforts...

**Hint**: We have not specified above how to derive plaintext blocks
from ciphertext blocks.  If you get stuck defining `decrypt`, using the
above equalities, you can work backward to solve for each plaintext
block.  You will need simple properties about `(^)` and a very
important property of (useful) block ciphers, defined as
`block_recovery` below.

```cryptol
// property block_recovery key pt = decipher key (encipher key pt) == pt
// property xor_inv_r x y = (x ^ y) ^ y == x
// property xor_inv_l x y = x ^ (x ^ y) == y
```

Working backward from our generator expression for `encrypt`...

`ct@i = encipher key (pt@i ^ (if i == 0 then iv else ct@(i-1)))`
(Apply `decipher key` to both sides.)
`decipher key (ct@i) = decipher key (encipher key (pt@i ^ (if i == 0 then iv else ct@(i-1))))`
(Apply `block_recovery` on right.)
`decipher key (ct@i) = pt@i ^ ct'`
(Apply `(^) ct'` to both sides, where `ct'` is the `if`-expression.)
`decipher key (ct@i) ^ ct' = pt@i ^ ct' ^ ct'
(Apply `xor_inv_r` on right.)
`decipher key (ct@i) ^ ct' = pt@i`
(Flip sides for assignment to plaintext.)
`pt@i = decipher key (ct@i) ^ ct'`
(Substitute the `if`-expression back in for `ct'`.)
`pt@i = decipher key (ct@i) ^ (if i == 0 then iv else ct@(i-1)))`

**Note**: Could you define `decrypt` without an initialization vector?

As with `ECB`, we can now define a `CBC` mode for various Simon and
Speck configurations, or any other simple block cipher that implements
our interface.

**Exercise**: Define submodules for each Simon and Speck parameter set
in CBC mode.

```cryptol
// /** Simon 32/64 in CBC mode */
// submodule CBC_Simon_32_64 = submodule ... { submodule ... }  // Replace with your definition and uncomment.

// Repeat for other Simon and Speck parameter sets.
```

**Exercise**: Likewise, define a submodule that implements DES (a weak
block cipher) in CBC (a cipher mode).  Add a docstring with a prominent
warning not to use it in production.

```cryptol
// /** Add a docstring. */
// submodule CBC_DES = ...  // Replace with your definition and uncomment.
```

Your solution should pass the following test vectors [3]:

```cryptol
submodule CBC_DES_Test where
  import submodule CBC_DES as CBC_DES

  e (key, iv, pt, ct) = CBC_DES::encrypt key iv pt == ct
  d (key, iv, pt, ct) = CBC_DES::decrypt key iv ct == pt
  
  // from FIPS 81 (withdrawn, of course)
  v_fips_81 = (0x0123456789abcdef, 0x1234567890abcdef, [0x4e6f772069732074, 0x68652074696d6520, 0x666f7220616c6c20], [0xe5c7cdde872bf27c, 0x43e934008c389c0f, 0x683788499a7c05f6])
  
  property e_fips_81 = e v_fips_81
  property d_fips_81 = d v_fips_81
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :check CBC_DES_Test::e_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :check CBC_DES_Test::d_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Cipher Feedback (CFB)

CFB can take any of various forms, including a full-block mode or one
that segments block into *subblocks*.  In the full-block configuration,
CFB is similar to CBC except that the XOR and cipher operations are
performed in the other order, i.e.:

`c0 = (encipher key iv) ^ pt0`
`c1 = (encipher key c0) ^ pt1`
`c2 = (encipher key c1) ^ pt2`
`...`

**Exercise**: Define CFB full-block mode for a given block cipher.

```cryptol
submodule CFB where
  // request a block cipher
  import interface submodule I_BlockCipher

  // derive the type aliases for that block cipher
  import submodule F_BlockCipher_Aliases { I = interface I_BlockCipher } (Block)

  /** Encrypt a multi-block message using CFB mode */
  encrypt : {n} fin n => Key -> Block -> [n]Block -> [n]Block
  encrypt key iv pt = undefined  // Replace with your definition.

  /** Decrypt a multi-block message using CFB mode */
  decrypt : {n} fin n => Key -> Block -> [n]Block -> [n]Block
  decrypt key iv ct = undefined  // Replace with your definition.
```

**Hint**: It is not necessary for a functor to use all the fields
in its interface(s).  Asking for unused fields is perhaps questionable,
but being able to reuse the same interface might turn out to justify
an ignored field...

**Hint**: The different order of operations has a profound impact on
CFB `decrypt` that might seem counterintuitive at first.  Again, if
you get stuck defining `decrypt`, work out from the above definition
how to solve for plaintext blocks in terms of ciphertext.  That is,
given `ct_i = (encipher key iv) ^ pt_i`, solve for `pt_i`.

**Exercise**: Define a submodule that implements DES (a weak block
cipher) in full-block CFB mode (a cipher mode).  Add a docstring with
a prominent warning not to use it in production.

**Solution**:

```cryptol
// /** ... */
// submodule CFB_DES = ...  // Replace with your definition and uncomment.
```

Your solution should pass the following test vectors [3]:

```cryptol
submodule CFB_DES_Test where
  import submodule CFB_DES as CFB_DES

  e (key, iv, pt, ct) = CFB_DES::encrypt key iv pt == ct
  d (key, iv, pt, ct) = CFB_DES::decrypt key iv ct == pt
  
  // from FIPS 81 (withdrawn, of course)
  v_fips_81 = (0x0123456789abcdef, 0x1234567890abcdef, [0x4e6f772069732074, 0x68652074696d6520, 0x666f7220616c6c20], [0xf3096249c7f46e51, 0xa69e839b1a92f784, 0x03467133898ea622])
  
  property e_fips_81 = e v_fips_81
  property d_fips_81 = d v_fips_81
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :check CFB_DES_Test::e_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :check CFB_DES_Test::d_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

## Counter (CTR) Mode (Multiple Interfaces and Interface Constraints)

Counter mode produces a stream cipher by applying a block cipher to
successive values of a "counter" that produces a long sequence of
non-repeating values -- most commonly a simple incremental counter.

This counter is concatenated with a *nonce*, which acts much as the
initialization vector from other modes into blocks, which are
enciphered with the key and then XOR'ed (or another invertible
operation) with each plaintext block:

`c0 = encipher key (nonce # 0) ^ p0`
`c1 = encipher key (nonce # 1) ^ p1`
`c2 = encipher key (nonce # 2) ^ p2`

Our CTR specification will need to define and import an interface for
the additional nonce-specific parameters, and constrain these such
that a nonce and counter can be concatenated into a `Block`.  Cryptol's
new module system can specify an
[`interface constraint`](https://galoisinc.github.io/cryptol/master/Modules.html#interface-constraints)
for parameters provided for one or more interfaces, e.g.

```cryptol
// import interface submodule I
// import interface submodule J
// interface constraint I::n == J::n
```

**Exercise**: Define CTR mode for a given block cipher and CTR-specific
parameters.

```cryptol
interface submodule I_CTR where
  // nonce width
  type w: #
  
  // counter width
  type c: #

submodule F_CTR_Aliases where
  import interface submodule I_CTR as I
  
  // type alias for nonce
  type Nonce = [I::w]

submodule CTR where
  // request a block cipher
  import interface submodule I_BlockCipher
  // request parameters for CTR
  import interface submodule I_CTR
  
  // interface constraint ...  // Define constraints over parameters in `I_BlockCipher` and `I_CTR`, and uncomment.
  
  // derive the type aliases for that block cipher, and for the nonce
  import submodule F_BlockCipher_Aliases { I = interface I_BlockCipher } (Block)
  import submodule F_CTR_Aliases { I = interface I_CTR } (Nonce)
  
  /** Encrypt a multi-block message using CFB mode */
  encrypt : {n} c >= width (max 1 n - 1) => Key -> Nonce -> [n]Block -> [n]Block
  encrypt key nonce pt = undefined  // Replace with your definition.
  
  /** Decrypt a multi-block message using CFB mode */
  decrypt : {n} c >= width (max 1 n - 1) => Key -> Nonce -> [n]Block -> [n]Block
  decrypt key nonce ct = undefined  // Replace with your definition.
```

**Hint**: Use generator expressions again.

**Exercise**: Find test vectors for a block cipher in CTR mode, specify
it, and verify the test vectors.

## ...and more!

**Exercise**: Feel free to specify other basic block cipher modes
such as PCBC, OFB, segmented CFB, etc.

**TODO**: Introduce authenticated cipher modes, a positive
authentication property, and resilience properties (tamper, spoofing).

**TODO**: Get to the point where (sub)modules can import interfaces and
specify properties assumed about their implementations.  Use these to
generate "contracts" and reusable test cases.

# Properties and Verification (Interface Reuse)

[2] defines the *CIA triad* of:
  - *confidentiality*
  - *integrity*
  - *availability*

In these terms, a cipher's purpose is to provide confidentiality (for
authorized persons to securely exchange message so that they are not
easily recoverable by unauthorized persons) and availability (messages
need to remain readable by authorized persons).

In particular, at the expense of repeating ourselves, a cipher must
satisfy this familiar property, which you may have already used to
understand how to specify `decrypt` for block cipher modes:

```cryptol
// property block_recovery key pt = decipher key (encipher key pt) == pt
```

In other words, a cipher must remain *available* to those with a shared
symmetric key.  This must also hold true for cipher modes, e.g. for a
cipher mode with an initialization vector:

```cryptol
// property mode_recovery = decrypt key iv (encrypt key iv pt) == pt
```

Another prerequisite of availability is that authorized parties agree
on how the cipher operates.  Cryptol expresses algorithms as rigorous
formal specifications, but users more commonly confirm this agreement
using *test vectors*, e.g.:

```cryptol
import submodule P_Simon_32_64_BlockCipher as S_32_64
import submodule F_BlockCipher_Aliases { submodule P_Simon_32_64_BlockCipher } as S_32_64

S_32_64_test_key = 0x94eeea8b1f2ada84
S_32_64_test_pt = 0xadf10331
S_32_64_test_ct = 0x391e7f1a

property test_simon_32_64_enc = S_32_64::encipher S_32_64_test_key S_32_64_test_pt == S_32_64_test_ct
property test_simon_32_64_dec = S_32_64::decipher S_32_64_test_key S_32_64_test_pt == S_32_64_test_ct
```

A strong cipher must resist cryptanalytic attacks of varying
sophistication.  Cryptol and SMT solvers are not designed to perform
rigorous cryptanalysis, but can run some quick sanity checks.

For example, it should not be easy to (ab)use SMT solvers to quickly
recover a cryptographic key from known or chosen plaintext:

```xcryptol-session
// Try to recover `test_key` from known test plaintext/ciphertext
// labs::ModuleSystem> :sat \key -> S_32_64::encipher key test_pt == test_ct
```

**TODO**: Introduce exercises to check `mode_recovery` for various
block cipher modes, and show how to use SAW to formally verify these
using a block cipher's `block_recovery` property when applicable.

# Conclusion

In this module, you learned how to define and reuse interfaces and
functors as the cryptographic building blocks (pun intended) of a
complex nested module with multiple ciphers, modes, and properties.

# References

[1] NIST SP 800-38A.
    "Recommendation for Block Cipher Modes of Operation: Tools and Techniques".
    December 2001.
    https://csrc.nist.gov/pubs/sp/800/38/a/final

[2] NIST SP 800-12 Rev. 1.
    "An Introduction to Information Security".
    June 2017.
    https://csrc.nist.gov/pubs/sp/800/12/r1/final

[3] NIST FIPS 81 (withdrawn).
    "DES MODES OF OPERATION"
    2 December 1980.
    https://csrc.nist.gov/pubs/fips/81/final

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [- Key Wrapping](../KeyWrapping/KeyWrapping.md) ||
| [< Parameterized Modules](../SimonSpeck/SimonSpeck.md) | **New Module System** ||
|| [! New Module System (Answers)](./NewModuleSystemAnswers.md) ||
