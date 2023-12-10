# Introduction

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in associated files.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * writing functions and properties,
  * testing properties by using the `:prove` and `:exhaust` commands,
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
[**Parameterized Modules**](../SimonSpeck/SimonSpeck.md) lab.  Some
exercises will require you to define submodules, after which you will
have to uncomment any test vectors that depend on these.

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
mode's (sub)module with a `parameter` block (commented out here because
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
(sub)module with the parameters in this block (along with any others
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
/* Not quite ready to define these modes yet...

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
(copied) type aliases.  This also demonstrates an *interface alias*
to distinguish the interface's type aliases (which would otherwise
conflict despite not being accessible) from the functor's:

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

Of course, parameterizable block cipher modes need block ciphers!  In
the previous lab on
[Parameterized Modules](../SimonSpeck/SimonSpeck.md), we implemeented
the Simon and Speck block ciphers, which are themselves parameterized.
To use these for our block cipher modes, we just need to adapt them to
our fancy new interface...

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

`submodule P_Simon_64_128_BlockCipher = submodule F_SimonSpeck_BlockCipher { labs::SimonSpeck::Simon::simon_64_128 }`

Start with explicitly named submodules. These can always be
"anonymized" later if it is obvious they won't be reused.

```cryptol
/** interface to collect relevant definitions from Simon instance */
interface submodule I_SimonSpeck_Inst where
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
  import interface submodule I_SimonSpeck_Inst as S

  // export definitions for `I_BlockCipher` parameters
  type Key = [S::keySize]
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

In the last exercise, you adapted your own prior work to fit our new
`I_BlockCipher` interface.  We can also adapt other simple block
ciphers, such as DES in the
[Cryptographic Proofs](../CryptoProofs/CryptoProofs.md) lab or
AES in the [Key Wrapping](../KeyWrapping/KeyWrapping.md) lab.

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

**Exercise**: Specify an interface and a functor to generate adaptors
for each `AES` key size that provide all parameters required by
`interface submodule I_BlockCipher`:

```cryptol
// Define and uncomment
/*
interface submodule I_AES_BlockCipher where
  // ...

submodule F_P_AES_BlockCipher where
  import interface submodule I_AES_BlockCipher as I

  // ...

submodule P_AES_128_BlockCipher = submodule F_P_AES_BlockCipher where
  // ...

submodule P_AES_192_BlockCipher = submodule F_P_AES_BlockCipher where
  // ...

submodule P_AES_256_BlockCipher = submodule F_P_AES_BlockCipher where
  // ...
*/
```

**Hint**: The interface could take the same key size parameter as
`specs::Primitive::Symmetric::Cipher::Block::AES::Algorithm`, and the
functor could define the corresponding key type, the same block size
for each key size, and `encipher` and `decipher`, using functions from
`specs::Primitive::Symmetric::Cipher::Block::AES_parameterized`.


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
no intermediate transformations to **diffuse** the message -- a
weakness that disqualifies it for use in security-critical settings.
But just as a pedagogical exercise, we can define ECB in Cryptol...

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
/* Uncomment after finishing your definition of `F_SimonSpeck_BlockCipher`
/** Simon 32/64 in ECB mode */
submodule ECB_Simon_32_64 = submodule ECB { submodule P_Simon_32_64_BlockCipher }
*/

// /** Simon 48/72 in ECB mode */
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

Your solution should pass the following test vectors [2]:

```cryptol
/* Uncomment after defining `ECB_DES`
/** ECB/DES Tests */
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
*/
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :exhaust ECB_DES_Test::e_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust ECB_DES_Test::d_fips_81
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

**Exercise**: Now define submodules that implement AES (a stronger
block cipher) in ECB (a weak cipher mode).  Add docstrings with
prominent warnings not to use them in production.

/* Uncomment after defining submodules
// /** Add a docstring */
submodule ECB_AES_128 = // ...
// /** Add a docstring */
submodule ECB_AES_192 = // ...
// /** Add a docstring */
submodule ECB_AES_256 = // ...
*/

Your solution should pass the following test vectors [3]:

```cryptol
// Let's use the new module system to generate groups of related properties...

submodule F_KAT_ECB where
  import interface submodule I_BlockCipher
  interface constraint fin b

  import submodule F_BlockCipher_Aliases { interface I_BlockCipher }

  interface submodule I_KAT where
    type n : #
    type constraint fin n

    KEY: Key
    PLAINTEXT : [n]Block
    CIPHERTEXT : [n]Block

  submodule F_KAT where
    import interface submodule I_KAT

    import submodule ECB { interface I_BlockCipher }
	
    property test_encrypt = encrypt KEY PLAINTEXT == CIPHERTEXT
    property test_decrypt = decrypt KEY CIPHERTEXT == PLAINTEXT

submodule P_KAT_ECB_AES_128_ENCRYPT_9 where
  type n = 10
  KEY = 0xebea9c6a82213a00ac1d22faea22116f
  PLAINTEXT = split 0x451f45663b44fd005f3c288ae57b383883f02d9ad3dc1715f9e3d6948564257b9b06d7dd51935fee580a96bbdfefb918b4e6b1daac809847465578cb8b5356ed38556f801ff7c11ecba9cdd263039c15d05900fc228e1caf302d261d7fb56cee663595b96f192a78ff4455393a5fe8162170a066fdaeac35019469f22b3470686bced2f007a1a2e43e01b4562caaa502ed541b8205874ec1ffb1c8b255766942
  CIPHERTEXT = split 0x01043053f832ef9b911ed387ba577451e30d51d4b6b11f319d4cd539d067b7f4f9b4f41f7f3d4e920c57cbe2b5e1885aa66203ae493e93a1df63793a9563c176bc6775dd09cc9161e278a01beb8fd8a19200326bd95abc5f716768e34f90b50523d30fdabb103a3bc020afbbb0cb3bd2ad512a6fea79f8d64cef347458dec48be89451cb0b807d73593f273d9fc521b789a77524404f43e00f20b3b77b938b1a

submodule P_KAT_ECB_AES_128_DECRYPT_9 where
  type n = 10
  KEY = 0x44f0ee626d0446e0a3924cfb078944bb
  CIPHERTEXT = split 0x931b2f5f3a5820d53a6beaaa6431083a3488f4eb03b0f5b57ef838e1579623103bd6e6800377538b2e51ef708f3c4956432e8a8ee6a34e190642b26ad8bdae6c2af9a6c7996f3b6004d2671e41f1c9f40ee03d1c4a52b0a0654a331f15f34dce4acb96bd6507815ca4347a3de11a311b7de5351c9787c4538158e28974ffa83d8296dfe9cd09cd87f7bf4f54d97d28d4788799163408323943b3e72f5eab66c1
  PLAINTEXT = split 0x9c29eecb2de04254fafb896a994102d1da30ddb49d82728eb23dbd029901e9b75b3d0aee03f7a05f6c852d8fada0b5c28e8c9aed334fad11829df3dfadc5c2e471eb41af9e48a8a465e03d5ebdb0216915081f3b5a0ebb2308dfc2d28e5a8ba3f32adae4c3575921bc657b63d46ba5a618880ee9ad8af3fba5643a5026facd7d667ce599327f936cdda7e1bb742a33a019990b76be648a6ec725daed540ed9e7

submodule P_KAT_ECB_AES_192_ENCRYPT_9 where
  type n = 10
  KEY = 0x4f41fa4d4a25100b586551828373bcca5540c68e9bf84562
  PLAINTEXT = split 0x7c727bd3e7048e7a8995b7b1169ae4b5a55e854bb4f7a9576d7863ab2868731d307322dcca606e047343676f6af4d9cf6ebf2bf9c95d87848d233c931e7a60eff08fb959924cde1eec8699ebc57890e3887024ef47c89a550018788d1faa3250452e06f148af25f07bc613cd2f0e501a79d738d4361f28f34dbee24034e03367b6b8d34df3738ca3a86b9ebcb09e639bcb5e2f519f4a7a86fc7c41556404a95d
  CIPHERTEXT = split 0x922812ad5feacdf11fe7fdae96300149419e31cff54061b3c5ed27fdb8b50c9c0932b522a6c04e482499b011ef3c3e9dc56a1a61cfeb78b34032d26dbdc3cac51a3279bc934b9bce2d9c19bf858235613ba784e48e292d22c6b5a28e1d1bb860524fb7b5f9b3d9a5f4da66e340585bd2496fe6d6942db8d05d716fec03b17d19abb58b33332e24beaec7995d69525364fe139aa1fd62054668c58f23f1f94cfd

submodule P_KAT_ECB_AES_192_DECRYPT_9 where
  type n = 10
  KEY = 0x9cc24ea1f1959d9a972e7182ef3b4e22a97a87d0da7ff64b
  CIPHERTEXT = split 0x952f4546a8bf7166964917ece01bda3c6857e427cef5da0ff90b0e4bf44cf7ccfccfdf01d713dcf9673f01c87eaed52bf4aa046ff778558ea396dc9cd240716136386148a5c76378b3ffcd40864407b8e60b40a594e0619eddae3f6d6e3b15b86af231e1bae5ed2aa512e11da0e5572b67ffff934c36e585cfdd9f877045cb19c183b994bf74645862ffa726739aadcb9e10aaffc881c88ca3aa65b37f667bcb
  PLAINTEXT = split 0xb8bb5ce53a15aa6dfdf2cb61bc8e3617d1d0fefe9ba5d175550470e32397f6f3b3e65b43bded2b21e5c181d3c4c4c526c41ceab044289508458048b63352dfc379de373fd19a2c900c43524b75949e677cceda866f7f2bcc4844ef2e5dac5b804b4045e657c8156d1dcdb43cbf2f5e00a4f9255e3be2439436c4d0449a8d2c4c1a56bece98ea0fd68abaf12398039994aebffc692b9000e580479b4f4b28b5fe

submodule P_KAT_ECB_AES_256_ENCRYPT_9 where
  type n = 10
  KEY = 0x44a2b5a7453e49f38261904f21ac797641d1bcd8ddedd293f319449fe63b2948
  PLAINTEXT = split 0xc91b8a7b9c511784b6a37f73b290516bb9ef1e8df68d89bf49169eac4039650c4307b6260e9c4e93650223440252f5c7d31c26c56209cbd095bf035b9705880a1628832daf9da587a6e77353dbbce189f963235df160c008a753e8ccea1e0732aa469a97659c42e6e31c16a723153e39958abe5b8ad88ff2e89af40622ca0b0d6729a26c1ae04d3b8367b548c4a6335f0e5a9ec914bb6113c05cd0112552bc21
  CIPHERTEXT = split 0x05d51af0e2b61e2c06cb1e843fee3172825e63b5d1ce8183b7e1db6268db5aa726521f46e948028aa443af9ebd8b7c6baf958067ab0d4a8ac530ecbb68cdfc3eb93034a428eb7e8f6a3813cea6189068dfecfa268b7ecd5987f8cb2732c6882bbec8f716bac254d72269230aec5dc7f5a6b866fd305242552d400f5b0404f19cbfe7291fab690ecfe6018c4309fc639d1b65fcb65e643edb0ad1f09cfe9cee4a

submodule P_KAT_ECB_AES_256_DECRYPT_9 where
  type n = 10
  KEY = 0xc4a71e055a7254dda360693fe1be49f10faa6731c36dbaa6590b05974e185c5b
  CIPHERTEXT = split 0x2c487fa96f4090c56aa1b5be81918a934c9492878fb0cd686dcf8d17d86485454c51237bbd09205dcef1552f430dd098b9d827a694730c133a0222c77f540f9d5fc2d36af359583c9e3b49df884228a64de79b67f66207c8281360b99b214042ce61367ff97960e944453cd63679bb44708897d29bc5e70f9fc8f1f715143fbb00f7f5c1b7b161ec26d8d41d36fab0fa8a85c3ee6ce4d37007eb7a89d6753590
  PLAINTEXT = split 0x31fd5a307e279b2f34581e2c432379df8eccbaf79532938916711cd377540b9045373e47f2214b8f876040af733f6c9d8f03a7c58f8714d2fbb4c14af59c75b483adc718946ee907a18286cc4efd206789064b6f1b195f0d0d234468e4f00e6f1cad5cd3b9c0a643b3c0dd09280ff2e2a5929183409384dd72dc94e39687ea2b623d5d776700bd8b36e6130ffde966f134c4b1f35f29c5cc4a03297e1ccc9539

/* Uncomment after defining `P_AES_..._BlockCipher`
import submodule F_KAT_ECB { submodule P_AES_128_BlockCipher } as F_KAT_ECB_AES_128 (F_KAT)
import submodule F_KAT_ECB_AES_128::F_KAT { submodule P_KAT_ECB_AES_128_ENCRYPT_9 } as KAT_ECB_AES_128_ENCRYPT_9
import submodule F_KAT_ECB_AES_128::F_KAT { submodule P_KAT_ECB_AES_128_DECRYPT_9 } as KAT_ECB_AES_128_DECRYPT_9

import submodule F_KAT_ECB { submodule P_AES_192_BlockCipher } as F_KAT_ECB_AES_192 (F_KAT)
import submodule F_KAT_ECB_AES_192::F_KAT { submodule P_KAT_ECB_AES_192_ENCRYPT_9 } as KAT_ECB_AES_192_ENCRYPT_9
import submodule F_KAT_ECB_AES_192::F_KAT { submodule P_KAT_ECB_AES_192_DECRYPT_9 } as KAT_ECB_AES_192_DECRYPT_9

import submodule F_KAT_ECB { submodule P_AES_256_BlockCipher } as F_KAT_ECB_AES_256 (F_KAT)
import submodule F_KAT_ECB_AES_256::F_KAT { submodule P_KAT_ECB_AES_256_ENCRYPT_9 } as KAT_ECB_AES_256_ENCRYPT_9
import submodule F_KAT_ECB_AES_256::F_KAT { submodule P_KAT_ECB_AES_256_DECRYPT_9 } as KAT_ECB_AES_256_DECRYPT_9
*/
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_128_ENCRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_128_ENCRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_128_DECRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_128_DECRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_192_ENCRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_192_ENCRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_192_DECRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_192_DECRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_256_ENCRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_256_ENCRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_256_DECRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust KAT_ECB_AES_256_DECRYPT_9::test_decrypt
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
`decipher key (ct@i) ^ ct' = pt@i ^ ct' ^ ct'`  
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

Your solution should pass the following test vectors [2]:

```cryptol
/* Uncomment after defining `CBC_DES`
submodule CBC_DES_Test where
  import submodule CBC_DES as CBC_DES

  e (key, iv, pt, ct) = CBC_DES::encrypt key iv pt == ct
  d (key, iv, pt, ct) = CBC_DES::decrypt key iv ct == pt
  
  // from FIPS 81 (withdrawn, of course)
  v_fips_81 = (0x0123456789abcdef, 0x1234567890abcdef, [0x4e6f772069732074, 0x68652074696d6520, 0x666f7220616c6c20], [0xe5c7cdde872bf27c, 0x43e934008c389c0f, 0x683788499a7c05f6])
  
  property e_fips_81 = e v_fips_81
  property d_fips_81 = d v_fips_81
*/
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :exhaust CBC_DES_Test::e_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystem> :exhaust CBC_DES_Test::d_fips_81
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

**Exercise**: Now define submodules that implement AES in CBC mode for
each AES key size.

Your solution should pass the following test vectors [3]:

```cryptol
submodule F_KAT_CBC where
  import interface submodule I_BlockCipher
  interface constraint fin b

  import submodule F_BlockCipher_Aliases { interface I_BlockCipher }

  interface submodule I_KAT where
    type n : #
    type constraint fin n

    KEY: Key
    IV: Block
    PLAINTEXT : [n]Block
    CIPHERTEXT : [n]Block

  submodule F_KAT where
    import interface submodule I_KAT

    import submodule CBC { interface I_BlockCipher }
	
    property test_encrypt = encrypt KEY IV PLAINTEXT == CIPHERTEXT
    property test_decrypt = decrypt KEY IV CIPHERTEXT == PLAINTEXT

submodule P_KAT_CBC_AES_128_ENCRYPT_9 where
  type n = 10
  KEY = 0x2c14413751c31e2730570ba3361c786b
  IV = 0x1dbbeb2f19abb448af849796244a19d7
  PLAINTEXT = split 0x40d930f9a05334d9816fe204999c3f82a03f6a0457a8c475c94553d1d116693adc618049f0a769a2eed6a6cb14c0143ec5cccdbc8dec4ce560cfd206225709326d4de7948e54d603d01b12d7fed752fb23f1aa4494fbb00130e9ded4e77e37c079042d828040c325b1a5efd15fc842e44014ca4374bf38f3c3fc3ee327733b0c8aee1abcd055772f18dc04603f7b2c1ea69ff662361f2be0a171bbdcea1e5d3f
  CIPHERTEXT = split 0x6be8a12800455a320538853e0cba31bd2d80ea0c85164a4c5c261ae485417d93effe2ebc0d0a0b51d6ea18633d210cf63c0c4ddbc27607f2e81ed9113191ef86d56f3b99be6c415a4150299fb846ce7160b40b63baf1179d19275a2e83698376d28b92548c68e06e6d994e2c1501ed297014e702cdefee2f656447706009614d801de1caaf73f8b7fa56cf1ba94b631933bbe577624380850f117435a0355b2b

submodule P_KAT_CBC_AES_128_DECRYPT_9 where
  type n = 10
  KEY = 0x97a1025529b9925e25bbe78770ca2f99
  IV = 0xd4b4eab92aa9637e87d366384ed6915c
  CIPHERTEXT = split 0x22cdc3306fcd4d31ccd32720cbb61bad28d855670657c48c7b88c31f4fa1f93c01b57da90be63ead67d6a325525e6ed45083e6fb70a53529d1fa0f55653b942af59d78a2660361d63a7290155ac5c43312a25b235dacbbc863faf00940c99624076dfa44068e7c554c9038176953e571751dfc0954d41d113771b06466b1c8d13e0d4cb675ed58d1a619e1540970983781dc11d2dd8525ab5745958d615defda
  PLAINTEXT = split 0xe8b89150d8438bf5b17449d6ed26bd72127e10e4aa57cad85283e8359e089208e84921649f5b60ea21f7867cbc9620560c4c6238db021216db453c9943f1f1a60546173daef2557c3cdd855031b353d4bf176f28439e48785c37d38f270aa4a6faad2baabcb0c0b2d1dd5322937498ce803ba1148440a52e227ddba4872fe4d81d2d76a939d24755adb8a7b8452ceed2d179e1a5848f316f5c016300a390bfa7

submodule P_KAT_CBC_AES_192_ENCRYPT_9 where
  type n = 10
  KEY = 0x162ad50ee64a0702aa551f571dedc16b2c1b6a1e4d4b5eee
  IV = 0x24408038161a2ccae07b029bb66355c1
  PLAINTEXT = split 0xbe8abf00901363987a82cc77d0ec91697ba3857f9e4f84bd79406c138d02698f003276d0449120bef4578d78fecabe8e070e11710b3f0a2744bd52434ec70015884c181ebdfd51c604a71c52e4c0e110bc408cd462b248a80b8a8ac06bb952ac1d7faed144807f1a731b7febcaf7835762defe92eccfc7a9944e1c702cffe6bc86733ed321423121085ac02df8962bcbc1937092eebf0e90a8b20e3dd8c244ae
  CIPHERTEXT = split 0xc82cf2c476dea8cb6a6e607a40d2f0391be82ea9ec84a537a6820f9afb997b76397d005424faa6a74dc4e8c7aa4a8900690f894b6d1dca80675393d2243adac762f159301e357e98b724762310cd5a7bafe1c2a030dba46fd93a9fdb89cc132ca9c17dc72031ec6822ee5a9d99dbca66c784c01b0885cbb62e29d97801927ec415a5d215158d325f9ee689437ad1b7684ad33c0d92739451ac87f39ff8c31b84

submodule P_KAT_CBC_AES_192_DECRYPT_9 where
  type n = 10
  KEY = 0x509baf46fb9de34281dafcc3db79593bffa8426904302688
  IV = 0xd6d86e0c82dd8788f4147a26f9a71c74
  CIPHERTEXT = split 0x6928299c52b4f047926f8a541529da2d6bbaa399143ced8efb77ab47409d9a953a386c7abd6026f49831c717627c2a5e77bd2d433d4d130dacd927ea0d13a23d01a7cf39c6716dafb6ed552410ef5d27fb947be2c8782eee7829196c7edcf151c65f9a01f54f8d20f38b7da4a7e83a2f0127d59d3e2405d8674fc9f41b604f788f4715f9d3624eee57f387bfadd18a1f905e839c26b8617482347fab6d08845a
  PLAINTEXT = split 0x67d2dda6da26e21307973400600725727ae81415511772f4a09ad9903bcf90cc2c0dac58ba559a0109c54a9d6117b15bb574ca473e848047e9a54ee4abde76aff9849c44109d161f46442e1610d8b015cf36a010ed8efa3207fdfc8fcc548f145c027e44c5b0ec35c9886f4b9d6513a5bc10d0ea6bbbc26f54b183bcae27fb799d8872ff748fc459d55cfa255aae29d71b076d9b44c14d5ceba9332a763d9c94

submodule P_KAT_CBC_AES_256_ENCRYPT_9 where
  type n = 10
  KEY = 0x48be597e632c16772324c8d3fa1d9c5a9ecd010f14ec5d110d3bfec376c5532b
  IV = 0xd6d581b8cf04ebd3b6eaa1b53f047ee1
  PLAINTEXT = split 0x0c63d413d3864570e70bb6618bf8a4b9585586688c32bba0a5ecc1362fada74ada32c52acfd1aa7444ba567b4e7daaecf7cc1cb29182af164ae5232b002868695635599807a9a7f07a1f137e97b1e1c9dabc89b6a5e4afa9db5855edaa575056a8f4f8242216242bb0c256310d9d329826ac353d715fa39f80cec144d6424558f9f70b98c920096e0f2c855d594885a00625880e9dfb734163cecef72cf030b8
  CIPHERTEXT = split 0xfc5873e50de8faf4c6b84ba707b0854e9db9ab2e9f7d707fbba338c6843a18fc6facebaf663d26296fb329b4d26f18494c79e09e779647f9bafa87489630d79f4301610c2300c19dbf3148b7cac8c4f4944102754f332e92b6f7c5e75bc6179eb877a078d4719009021744c14f13fd2a55a2b9c44d18000685a845a4f632c7c56a77306efa66a24d05d088dcd7c13fe24fc447275965db9e4d37fbc9304448cd

submodule P_KAT_CBC_AES_256_DECRYPT_9 where
  type n = 10
  KEY = 0x87725bd43a45608814180773f0e7ab95a3c859d83a2130e884190e44d14c6996
  IV = 0xe49651988ebbb72eb8bb80bb9abbca34
  CIPHERTEXT = split 0x5b97a9d423f4b97413f388d9a341e727bb339f8e18a3fac2f2fb85abdc8f135deb30054a1afdc9b6ed7da16c55eba6b0d4d10c74e1d9a7cf8edfaeaa684ac0bd9f9d24ba674955c79dc6be32aee1c260b558ff07e3a4d49d24162011ff254db8be078e8ad07e648e6bf5679376cb4321a5ef01afe6ad8816fcc7634669c8c4389295c9241e45fff39f3225f7745032daeebe99d4b19bcb215d1bfdb36eda2c24
  PLAINTEXT = split 0xbfe5c6354b7a3ff3e192e05775b9b75807de12e38a626b8bf0e12d5fff78e4f1775aa7d792d885162e66d88930f9c3b2cdf8654f56972504803190386270f0aa43645db187af41fcea639b1f8026ccdd0c23e0de37094a8b941ecb7602998a4b2604e69fc04219585d854600e0ad6f99a53b2504043c08b1c3e214d17cde053cbdf91daa999ed5b47c37983ba3ee254bc5c793837daaa8c85cfc12f7f54f699f

/* Uncomment after defining `P_AES_..._BlockCipher`
import submodule F_KAT_CBC { submodule P_AES_128_BlockCipher } as F_KAT_CBC_AES_128 (F_KAT)
import submodule F_KAT_CBC_AES_128::F_KAT { submodule P_KAT_CBC_AES_128_ENCRYPT_9 } as KAT_CBC_AES_128_ENCRYPT_9
import submodule F_KAT_CBC_AES_128::F_KAT { submodule P_KAT_CBC_AES_128_DECRYPT_9 } as KAT_CBC_AES_128_DECRYPT_9

import submodule F_KAT_CBC { submodule P_AES_192_BlockCipher } as F_KAT_CBC_AES_192 (F_KAT)
import submodule F_KAT_CBC_AES_192::F_KAT { submodule P_KAT_CBC_AES_192_ENCRYPT_9 } as KAT_CBC_AES_192_ENCRYPT_9
import submodule F_KAT_CBC_AES_192::F_KAT { submodule P_KAT_CBC_AES_192_DECRYPT_9 } as KAT_CBC_AES_192_DECRYPT_9

import submodule F_KAT_CBC { submodule P_AES_256_BlockCipher } as F_KAT_CBC_AES_256 (F_KAT)
import submodule F_KAT_CBC_AES_256::F_KAT { submodule P_KAT_CBC_AES_256_ENCRYPT_9 } as KAT_CBC_AES_256_ENCRYPT_9
import submodule F_KAT_CBC_AES_256::F_KAT { submodule P_KAT_CBC_AES_256_DECRYPT_9 } as KAT_CBC_AES_256_DECRYPT_9
*/
```

```xcryptol-session
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_128_ENCRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_128_ENCRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_128_DECRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_128_DECRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_192_ENCRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_192_ENCRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_192_DECRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_192_DECRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_256_ENCRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_256_ENCRYPT_9::test_decrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_256_DECRYPT_9::test_encrypt
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
labs::NewModuleSystem::NewModuleSystemAnswers> :exhaust KAT_CBC_AES_256_DECRYPT_9::test_decrypt
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
a prominent warning not to use it in production.  Check these against
test vectors in [2].

```cryptol
// /** ... */
// submodule CFB_DES = ...  // Replace with your definition and uncomment.
```

**Exercise**: Now define submodules that implement AES in CFB mode for
each AES key size, and verify these against some test vectors in [3].

```cryptol
// /** ... */
// submodule CFB_AES_128 = ...  // Replace with your definition and uncomment.
// /** ... */
// submodule CFB_AES_192 = ...  // Replace with your definition and uncomment.
// /** ... */
// submodule CFB_AES_256 = ...  // Replace with your definition and uncomment.
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

# Conclusion

In this module, you learned how to define and reuse interfaces and
functors as the cryptographic building blocks (pun intended) of a
complex nested module with multiple ciphers, modes, and properties.

# References

[1] NIST SP 800-38A.
    "Recommendation for Block Cipher Modes of Operation: Tools and Techniques".
    December 2001.
    https://csrc.nist.gov/pubs/sp/800/38/a/final

[2] NIST FIPS 81 (withdrawn).
    "DES MODES OF OPERATION"
    2 December 1980.
    https://csrc.nist.gov/pubs/fips/81/final

[3] NIST Advanced Encryption Standard Algorithm Verification System
    AES Multiblock Message Test (MMT) Sample Vectors
	https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/aes/aesmmt.zip

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
