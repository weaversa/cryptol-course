# TWINE: A Case Study for Cryptol's Parameterized Modules

This folder specifies a [block cipher](https://en.wikipedia.org/wiki/Block_cipher) ([TWINE](https://www.nec.com/en/global/rd/tg/code/symenc/pdf/twine_LC11.pdf))* running in a [block cipher mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation) ([CBC](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher_block_chaining_(CBC))) in Cryptol using parameterized modules, an experimental feature that enables related specifications to reuse common parameters.

TWINE was selected for...reasons. (The Simon/Speck documentation compares other lightweight block ciphers, and no prior Cryptol specification was found online at the time of writing -- what more reason do we need?)  However, it serves as an excuse to explore how parameterized modules can help (and/or hinder) efforts to reuse common artifacts among related specifications.  Because parameterized modules are experimental, changes to their implementation are very likely to affect any observations noted here.  (And there are undoubtedly better approaches than this one.)

Parameterized modules by definition involve multiple Cryptol modules, so this Markdown file is ***not*** a (valid) Literate Cryptol specification.  It will refer to Cryptol modules in other files and demonstrate interactions with the Cryptol interpreter.

## Background

### Parameterized Modules

As previously stated, parameterized modules are an experimental feature of Cryptol that enables related specifications to reuse common parameters.  Abstractly, a parameterized module has the following form:

```Cryptol
module {Name} where

parameter
    // type parameters and constraints
    type {Name}: {Kind}
    type constraint ...{Name}...
    ...

    // value parameters
    {name}: {Signature}
    ...

// exported definitions

type {Name} {Args} = {Type}
type constraint {Name} {Args} = {Constraints}

{name}: {TypeArgs} {Constraints} => {Signature}
{name} {args} = {value}

private
    // private definitions
    ...
```

`{Kind}` can be a basic type (e.g. `type Block: [64]`), a previously defined custom type (e.g. `type Blocks: [n]Block`), or a wildcard denoting a numeric type (e.g. `type numRounds: #`) or any type (e.g. `type Key: *`).  Constraints can be declared for parameter types via a parametric type constraint, e.g. `type constraint (fin numRounds)` or `type constraint (Cmp Block, Logic Block)`.  Value parameters take the same form as the signature part of type annotations for regular values, e.g. `encrypt: Key -> Block -> Block`.  Other modules can then reuse a parameterized module via instantiation or importation:

#### Instantiation

A named instantiation has the following form:

```Cryptol
module {Name} = {Base} where

parameter
    // parameter declarations for this module
    ...

// parameter instantiations for the base module (must satisfy base constraints)
type {BaseName} = {Type}
{basename} {args} = {value}

// other definitions that are not accessible to instantiating or importing modules
// (the interpreter can see these when this or an instantiating (but not importing) module is loaded)
type {Name} = {Type}

private
    // still more definitions that are not accessible to instantiating or importing modules
    // (the interpreter can only see these when this module is loaded)
```

As stated in *Programming Cryptol*:
> Note that the only purpose of the body of [the instance module] (the declarations after the where keyword) is to define the parameters
for [the parameterized module].

In other words, any definitions introduced outside parameter scope will not be visible to other modules.  To get around this, it is common to specify a related non-parameterized module with definitions to be exported, and often a third that combines the parameterized and non-parameterized definitions.  An alternative is to just import the base module; see below.

#### Parameterized Importation ####

A parameterized import is similar to an ordinary import: ``import `{Name}``.  Quoting *Programming Cryptol* again:
> In this case, Cryptol will import all definitions from the module as usual, however every definition will have some
additional parameters corresponding to the parameters of a module. All value parameters are grouped in a record.

This is useful when some but not all of the definitions from the parameterized module are needed.  Imported type definitions are of the form `{Name} {Parameters}`, while values are ``{name}`{ParamTypes} {paramArgs} {args}``, e.g.

```Cryptol
module Base where

parameter
    type P1: *
    type P2: *

    p1: P1
    p2: P2

type T = (P1, P2)

v = (p1, p2)
```
```Cryptol
module Importer where

import `Base as Base

type I1 = [4]
type I2 = [8]

i1 = 13: I1
i2 = 37: I2

type I = Base::T I1 I2

i = Base::v`{I1, I2} {p1 = i1, p2 = i2}
```

This TWINE-CBC spec uses both instantiation and importation where appropriate.

### Prior Work

The [`cryptol-specs`](https://github.com/GaloisInc/cryptol-specs) is the *de facto* reference for Cryptol specifications.  It includes non-parameterized specs for [block ciphers](https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Symmetric/Cipher/Block/Cipher.cry) and [CBC mode](https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Symmetric/Cipher/Block/Modes/CBC.cry), and demonstrates use of the latter for a parameterized [AES](https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Symmetric/Cipher/Block/AES_parameterized.cry) spec.

## Goals

This TWINE-CBC specification aims to:
  * Be [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)
  * Define a reusable specification for block ciphers
  * Define a reusable specification for CBC mode
  * Check test vectors for TWINE-80 and TWINE-128 specs
  * Prove that TWINE and TWINE-CBC specs meet common crypto properties (e.g. plaintext recovery and uniqueness)
  * Demonstrate parameterized module instantiation and importation

### Notation

This spec annotates use of parameterized modules by introducing a `_`:
  * before and/or after a module name to denote that the module instantiates (a base module's) and/or declares (its own) parameters
  * before a parameter name to distinguish it from a body declaration
  * twice before an instance module's body declaration, which is sometimes visible in the interpreter but never accessible to other modules

Additionally, imported parameterized modules are aliased, retaining their `_` annotations, to see a given definition's source at a glance.

### Block Cipher

TWINE-CBC begins by defining a [block cipher](https://github.com/GaloisInc/cryptol-specs/blob/master/Primitive/Symmetric/Cipher/Block/Cipher.cry) with minimally secure cryptographic properties as a parameterized module [`Block_`](Block_.cry):
  * `parameter`s
    * `type _Key: *`, `type _Block: *`, `type constraint Cmp _Block`
      Numerous specifications, including [NIST 800-30A](https://csrc.nist.gov/publications/detail/sp/800-38a/final) and the `cryptol-specs` module upon which this example is based, specify the key and block to be bitstrings (in Cryptol, `[n]` for some bitlength `n`).  However, the functions and properties at this layer of abstraction allow for arbitrary types, provided that `_Block` is comparable (`Cmp`).
    * `_encrypt: Op` and `_decrypt: Op`, where `type Op = Key -> Block -> Block`, where `Key` and `Block` reflect the corresponding type parameters
      Block cipher operations have the same signature.  The module exports this derived signature type as well as mirrors of the underlying parameters (`Key` and `Block`).  By default, an instance module does not export the parameters with which it is instantiated; instead, these must be declared in the parameterized module in order to be accessible to modules importing or instantiating the instance module.  Unfortunately, though the type constraint on `Block` can also be exported (e.g. `type constraint isBlock = Cmp Block`), the interpreter would not mention the constraint when browsing `:help Block`.
  * exports
    * `type Key = _Key`, `type Block = _Block`, `type Op = Key -> Block -> Block`
      See above.
    * `encrypt = _encrypt`, `decrypt = _decrypt`
      `Block_` also exports the cipher operations mirroring its respective parameters.
    * `property recovery`, `property injectivity`
      `decrypt k` recovers plaintext `t` encrypted by `encrypt k`, and for different plaintext `t1` and `t2`, `encrypt k t1` differs from `encrypt k t2`; i.e. `encrypt k` is injective (and thus bijective because the domain `Block` is also the range).  These could be interpreted as "value constraints" on the parameters.  Unfortunately, `injectivity` is very time-consuming for SMT solvers to `:prove` (Boolector can do so in about 12 minutes.)

### Cipher Block Chaining (CBC) Mode

CBC is a block cipher mode of operation that encrypts multiple blocks by [XOR](https://en.wikipedia.org/wiki/Exclusive_or)ing the current plaintext block with the previous ciphertext block before encrypting with the underlying block cipher.  For this Cryptol spec, [`CBC_`](Block/Mode/CBC_.cry) is defined in terms of parameters reflecting the underlying block cipher and introduces the type constraint that blocks can be XORed:
  * ``import `labs::TWINE::Block_ as BlockCipher_``
    This module imports `Block_` with the alias `BlockCipher_`, to distinguish the similarly named module and type and to distinguish this module's own definitions from those in `Block_`
  * `parameter`s
    * `_BlockCipher_...`
      It would be nice to encapsulate the underlying block cipher as a parameter, but Cryptol does not yet provide such a feature.  Instead, the module settles for copying the parameters from `Block_`, prefacing them with `BlockCipher_` to distinguish them from any parameters exclusive to this module.
    * `type constraint Logic _BlockCipher_Block`
      The requirement that a block is XORable falls to this cipher mode.  Cryptol designates the `Logic` typeclass to denote items on which `^` and other bitwise operations (`&&`, `||`, etc.) can be performed.
  * exports
    * `BlockCipher_...`
      Again, because `Block_` can't be encapsulated in whole, this module reflects its exported definitions.  However, the `BlockCipher_` prefix is removed for components in common with CBC: `Key` and `Block`
    * `type Op n = Key -> Block -> [n]Block -> [n]Block`, `encrypt: Op`, `decrypt: Op`, `property recovery`, `property injectivity`
      This module coopts similar terminology for multi-block variants of respective single-block concepts.  Recovery can be verified quickly for small block sizes, but even ```injectivity{1}`` takes 19 minutes for Boolector (and much longer for Z3 and others).

### TWINE

TWINE is small family of two closely related lightweight block ciphers, TWINE-80 and TWINE-128, which differ in key size (and thus key schedules used to expand the key for block cipher operations) but otherwise share most of their features in common:
  * block size
  * expanded round key size
  * [subsitution-permutation network](https://en.wikipedia.org/wiki/Substitution%E2%80%93permutation_network)
    * an [S-box](https://en.wikipedia.org/wiki/S-box)
    * a [P-box](https://en.wikipedia.org/wiki/Permutation_box) (`π` in the written spec) and its inverse
  * encryption and decryption operations (`Enc` and `Dec`) that use the expanded round key (and that themselves reuse most of the same components of a [Generalized Feistel Structure](http://cryptowiki.net/index.php?title=Generalized_Feistel_networks))

Here, [`_TWINE_`](Block/_TWINE_.cry) is specified in a parameterized module that instantiates [`Block_`](Block_.cry):
  * `import labs::TWINE::Block::TWINE_Common as Common`
    It would be convenient to specify definitions shared by TWINE-80 and TWINE-128 in this instantiated module, but remembering that "the only purpose of the body of [the instance module] is to define the parameters for [the parameterized module]", the spec must instead migrate these and other common definitions to a non-parameterized module, [`TWINE_Common`](Block::TWINE_Common.cry).
  * `parameter`s
    * `type _k: #`, `type constraint 1 >= k`
      As previously stated, TWINE supports an 80- or 128-bit key.  Cryptol does not support [*disjunction*](https://en.wikipedia.org/wiki/Logical_disjunction) for type constraint definitions (while `=>` and `,` are curried and uncurried operators for type constraint [*conjunction*](https://en.wikipedia.org/wiki/Logical_conjunction), respectively), but such a limitation can be cleverly worked around for simple cases like this.  For TWINE in particular, its key size can be expressed as `80 + 48 * _k`, where `_k` is a numeric type (`#`) such that `1 >= k` (where `k >= 0` by definition).
    * `_keySchedule: Common::KeySchedule _k`
      Additionally, like many but not all other block ciphers, TWINE performs block encryption over a number of rounds, each of which uses a round key derived from the secret key using a [*key schedule*](https://en.wikipedia.org/wiki/Key_schedule).
  * other definitions
    * `type __Op = _Key -> Common::Block -> Common::Block`
      common type signature for block cipher operations
  * instantiations of `Block_` parameters
    * `type _Key = Common::Key _k`, `type _Block = Common::Block`
      Key size is derived from the parameter `_k` (as described above), whereas block size is common to TWINE-80 and TWINE-128.
    * `_encrypt key pt = Common::Enc (_keySchedule key) pt`, `_decrypt key ct = Common::Dec (_keySchedule key) ct`
      The written spec defines block cipher operations in terms of `Enc` and `Dec` functions, respectively, which are common to TWINE-80 and TWINE-128 but take round keys expanded from the secret key via distinct key schedules for the different key sizes.

Definitions common to TWINE-80 and TWINE-128 are defined in [`TWINE_Common`](Block/TWINE_Common.cry):
  * `type Key _k = [80 + 48 * _k]`
    actual key length in terms of `_k` parameter used to instantiate TWINE as TWINE-80 or TWINE-128, accordingly
  * `type RoundKey = [32*36]`, `type Block = [64]`
    (multi-)round key and block size; TWINE splits the key into 32-bit subkeys for each round, during which each subkey is further split into 4-bit nybbles
  * `CONS_HL: [35][2][4]`
    round constants defined (according to the written spec) as `2^^i` in `GF(2^^6)` with primitive polynomial `z^^6 + z + 1`, and split high/low as needed for block cipher operations; the written spec defines 36 such constants, but the last is not used
  * `updateXORs ...`
    a convenience function to update multiple items at specified indices of a sequence by XORing them with specified values in-place, e.g. (`updateXORS [3,4,5,6:[8]] [0,2] [10, 11]` would compute `[3 ^ 10, 4, 5 ^ 11, 6:[8]]`)
  * `S: [4] -> [4]`
    an S-box; see above
  * `pi: [16][5] -> [16][5]`, `pi': [16][5] -> [16][5]`
    a P-box and its inverse; see above
  * `property pi'_inverts_pi`, `property pi_inverts_pi'`
    `pi` and `pi'` invert each other
  * `Enc: RoundOp`, `Dec: RoundOp`, `type RoundOp = RoundKey -> Block -> Block`
    encryption and decryption operations based on expanded round key; these are common to TWINE-80 and TWINE-128
  * `property recovery`, `property injectivity`
    `Enc` and `Dec` satisfy the same `recovery` and `injectivity` properties as their counterparts

Lastly, the Cryptol spec defines instance modules for [`_TWINE_80`](Block/_TWINE_80.cry) and [`_TWINE_128`](Block/_TWINE_128.cry)
  * `import labs::TWINE::Block::TWINE_Common as Common`
    Reuses common definitions for TWINE; a real spec would likely not alias `Common`.
  * instantiations of `_TWINE_` parameters
    * `type _k = 0` or `type _k = 1`, respectively
      see above
    * `_keySchedule: Common::KeySchedule _k`
      key schedules that respectively expand different secret key lengths to the same round key length

We would like to show that our TWINE-80 and TWINE-128 specs respectively satisfy the test vectors provided in the written specification (all one of them for each key length).  This spec works around the problem by importing `_TWINE_80` and `_TWINE_128` into respective [`TWINE_80`](Block/TWINE_80.cry) and [`TWINE_128`](Block/TWINE_128.cry) modules:
  * `import labs::TWINE::Block::_TWINE_80` or `import labs::TWINE::Block::_TWINE_128`
    Import the instance module...
  * `property test`, `property test'`
    ...and add tests to check that encryption and decryption are consistent with the test vectors in the written specification.

### TWINE-CBC

Finally, the spec instantiates `CBC_` to define Cipher Block Chaining for TWINE-80 and TWINE-128 in respective [`_TWINE_80_CBC`](Block/Mode/_TWINE_80_CBC.cry) and [`_TWINE_128_CBC`](Block/Mode/_TWINE_128_CBC.cry) modules:
  * `import labs::TWINE::Block::_TWINE_80 as _TWINE_80` or `import labs::TWINE::Block::_TWINE_128 as _TWINE_128`
    Imports instance modules; as named instances themselves, importing the combined TWINE-80 or TWINE-128 module with test vectors would have no effect, as these definitions would not be forwarded.
  * instantiations of `CBC_` parameters
    * `type _BlockCipher_Key = _TWINE_80::Key`, `type _BlockCipher_Block = _TWINE_80::Block`, `_BlockCipher_encrypt = _TWINE_80::encrypt`, `_BlockCipher_decrypt = _TWINE_80::decrypt` or
      `type _BlockCipher_Key = _TWINE_128::Key`, `type _BlockCipher_Block = _TWINE_128::Block`, `_BlockCipher_encrypt = _TWINE_128::encrypt`, `_BlockCipher_decrypt = _TWINE_128::decrypt` or
      passes `CBC_` its needed `BlockCipher_` parameters from `_TWINE_80` or `_TWINE_128`, respectively

The spec combines block cipher and CBC properties and test vectors by unifying instance and common modules into single imports in [`TWINE_80_CBC`](Block/Mode/TWINE_80_CBC.cry) and [`TWINE_128_CBC`](Block/Mode/TWINE_128_CBC.cry):
  * `import labs::TWINE::Block::_TWINE_80`, `import labs::TWINE::Block::Mode::_TWINE_80_CBC` or
    `import labs::TWINE::Block::_TWINE_128`, `import labs::TWINE::Block::Mode::_TWINE_128_CBC`

## Synopsis

So how well did this spec meet the stated goals? ...

> This TWINE-CBC specification aims to:
>   * Be [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)

Nope.  Attempted reuse of the block cipher spec for CBC mode was very [WET](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself#DRY_vs_WET_solutions)...but where is the towel?  Reusing TWINE definitions for TWINE-80 and TWINE-128 also required a separate module.  There are numerous other instances where it would be nice to have access to an instance module's body definitions.

>   * Define a reusable specification for block ciphers

Check.  This layer of abstraction should apply to most block ciphers.  Whether it's as convenient as `cryptol-specs`' use of a record structure to represent a block cipher for CBC is a matter of taste.

>   * Define a reusable specification for CBC mode

Check.  This CBC spec should work for most block ciphers, but it lacks a means to conveniently encapsulate the block ciphers for instantiating modules.

>   * Check test vectors for TWINE-80 and TWINE-128 specs

Check...though these had to be delegated to another module.

>   * Prove that TWINE and TWINE-CBC specs meet common crypto properties (e.g. plaintext recovery and uniqueness)

Check...eventually.  Recovery is easy to `:prove` on a small number of blocks locally, but takes much longer on Docker images.  Injectivity for just one block appears laborious for most SMT solvers on any platform, with Boolector "only" taking minutes instead of hours or days.

>   * Demonstrate parameterized module instantiation and importation

Check...for better or worse.

So are parameterized modules worth it?  You decide!