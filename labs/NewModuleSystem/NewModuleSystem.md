# Introduction

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in associated files.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * parameterized modules
  * ...

## Skills You'll Learn

By the end of this lab you will have gained experience with Cryptol's
new module system, using it to implement a complex module with
multiple submodules and interfaces.

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
Loading module labs::NewModuleSystem::NewModuleSystem
```

We start by defining a new module for this lab:

```cryptol
module labs::NewModuleSystem::NewModuleSystem where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically. 
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Background

Before proceeding, recall a prior lab describing
[parameterized modules](../SimonSpeck/SimonSpeck.md)
for the SIMON and SPECK algorithms. That lab required us to split
each parameter setting into its own module:

> But because this lab introduces further concepts around the
> definition and use of modules, this lab will not be entirely
> self-contained in this file.

Another more subtle limitation is that it is tricky to specify
multiple layers of parameterized modules, so specifications often
resort to repeating parameters. For SIMON, block size is repeated
for each corresponding key size and number of rounds, but this is
minor because block size is a simple integer. But for more complex
specifications like
[SHA-2](https://github.com/GaloisInc/cryptol-specs/tree/master/Primitive/Keyless/Hash/SHA2),
complex parameters like `K` (a sequence of words of bitlength 32 or
64) are repeated for different digest lengths at each word size,
potentially introducing easy-to-miss inconsistencies.

# New Module System

These limitations motivated customers to ask Galois to
[update the module system](https://github.com/GaloisInc/cryptol/issues/815)
to support layered specifications, cipher modes, common interfaces,
and other such complexities. The long-awaited update will soon be (or
have been) [merged](https://github.com/GaloisInc/cryptol/pull/1363)
and
[documented](https://github.com/GaloisInc/cryptol/blob/master/docs/RefMan/Modules.rst).

## `submodule`: Nested Modules

The new module system supports `submodule`s that can be nested into
other (`sub`)`module`s:

```cryptol
  submodule Hash where
    submodule XXHash where
      TODO = undefined

    submodule SHA where
      TODO = undefined

      submodule SHA2 where
        TODO = undefined
```

This removes a previous limitation -- multiple parameterizations no
longer have to be specified in different modules.

## Interfaces, Parameterization, Abstraction, and Instantiation

In the new module system, an _interface_ specifies _parameters_ that an
_abstract_ (sub)module can _instantiate_ to produce an _implementation_.
Let's see this in a simple contrived example:

```cryptol
  submodule Contrivance where
    interface submodule I_n where
      type n : #
      type constraint (2 >= n, n >= 1)

    submodule A_n where
      import interface submodule I_n

      type w = 32*n
      type W = [w]

      z : W
      z = zero

    submodule P_1 where
      type n = 1

    submodule P_2 where
      type n = 2

    submodule Z_1 = submodule A_n { submodule P_1 }
    submodule Z_2 = submodule A_n { submodule P_2 }
  
    import submodule Z_1 as Z32
    import submodule Z_2 as Z64
  
    property zero_32 = Z32::z == zero`{[32]}
    property zero_64 = Z64::z == zero`{[64]}
```

The above snippet introduces a `submodule` `Contrivance`. Within, it
defines an `interface` with a numeric parameter `n` bound to the
interval `[1,2]`. An abstract submodule `A_n` requests a
parameterization satisfying `I_n`, and produces an implementation
defining `z`, a `zero` value of `type W = [w]`, where `type w = 32*n`.
Parameterizations `P_1` and `P_2` specify `n` values of `1` and `2`,
respectively, each satisfying `I_n`. Parameterizations `P_1` and `P_2`
instantiate `A_n` to define `Z_1` and `Z_2`, respectively, with `z` as
a corresponding `32`- or `64`-bit `zero`, as expressed by properties
`zero_32` and `zero_64`, respectively; each property requires the
instantiated submodules to be imported with aliases (here, `Z32` and
`Z64`) to distinguish their respective `z` definitions:

```Xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :prove Contrivance::zero_32
:prove Contrivance::zero_32
    Q.E.D.
(Total Elapsed Time: 0.035s, using "Z3")
labs::NewModuleSystem::NewModuleSystem> :prove Contrivance::zero_64
:prove Contrivance::zero_64
    Q.E.D.
(Total Elapsed Time: 0.031s, using "Z3")
```

All of the above were defined within the `Contrivance` submodule,
hence the leading `Contrivance::` in the property names.

## Scope

Only the top-level definitions `zero_32` and `zero_64` are in scope
after loading the top-level module from this point. Nested submodules
are not exported; to make `z` from each submodule available as well,
they must be reassigned explicitly:

```cryptol
    z32 = Z32::z
    z64 = Z64::z
```

```Xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :h Contrivance

Module Contrivance exports:
  
   
  zero_32 : Bit
  zero_64 : Bit
  z32 : W
  z64 : W
labs::NewModuleSystem::NewModuleSystem> Contrivance::z32
0x00000000
labs::NewModuleSystem::NewModuleSystem> Contrivance::z64
0x0000000000000000
labs::NewModuleSystem::NewModuleSystem> Contrivance::zero_32
True
labs::NewModuleSystem::NewModuleSystem> Contrivance::zero_64
True
```

# Practicum

Having introduced the new module system, let's apply it to SIMON and
compare the former and new module systems.

## Simon says what?

Recall that SIMON is parameterized on four constrained numeric (`#`)
`type`s:
  * `n` (word size)
  * `m` (words per key)
  * `T` (rounds in key schedule)
  * `j` (index of z-sequence)

**EXERCISE**: Define an `interface submodule` comprising these types
and their constraints.
(**Hint**: Just copy them from the previous lab.)

```cryptol
  submodule SIMON where
    /** interface specifying parameters for SIMON block cipher */
    interface submodule I_SIMON where
      /** word size */
      type n : #
      // TODO: Add constraint

      /** number of words in key */
      type m : #
      // TODO: Add constraint

      /** number of rounds in key schedule */
      type T : #
      // TODO: Add constraint

      /** index of z-sequence used (i.e. use sequence z_j) */
      type j : #
      // TODO: Add constraint
```

Recall that, given these parameters, SIMON exports:
  * derived `type`s:
    * `type blockSize = 2 * n`
    * `type keySize = m * n`
  * block cipher functions:
    * `encrypt : [keySize] -> [blockSize] -> [blockSize]`
    * `decrypt : [keySize] -> [blockSize] -> [blockSize]`
  * properties:
    * `EncryptDecryptIdentity : [keySize] -> [blockSize] -> Bool`
    * `DecryptEncryptIdentity : [keySize] -> [blockSize] -> Bool`

These exported definitions rely on numerous `private` ones.

Eventually, we will need to (re)implement `encrypt` and `decrypt`
functions in an abstract submodule that imports `I_SIMON`. Testing the
implementations will require parameterizations satisfying `I_n` to
instantiate the abstract submodule. So let's start there:

**EXERCISE**: Define submodules with parameters satisfying `I_n` for
each approved `n` (word size) and `m` (words per key), or just a few
if you're so inclined. (`T` and `j` depend on `n` and `m`.)
(**Hint**: For each `simon_2n_mn.cry`, copy its `parameter` block body
into a `submodule P_SIMON_2n_mn` (or `P_SIMON_n_m` if you prefer).

```cryptol
    submodule P_SIMON_32_64 where
      type n = 0
      type m = 0
      type T = 0
      type j = 0
    
    // TODO: Add remaining SIMON parameterizations
```

**EXERCISE**: Define an abstract `submodule` `A_SIMON` that requests a
parameterization satisfying `I_SIMON` and exports an implementation
with the above definitions, and instantiate this with each of the
above parameterizations into concrete submodules. `:check` your work.
(**Hint**: Just copy the previous definitions from `Simon.cry` from
the previous lab, and instatiate them using lines of the form
`submodule SIMON_2n_mn = submodule A_SIMON { submodule P_SIMON_2n_mn }` (or
likewise if you used another naming scheme earlier).

```cryptol
    /**
     * abstraction that produces a SIMON block cipher implementation
     * from parameters satisfying the `I_SIMON` interface
     */
    submodule A_SIMON where
      import interface submodule I_SIMON
      TODO = undefined  // Replace with abstract SIMON implementation

    submodule SIMON_32_64   = submodule A_SIMON { submodule P_SIMON_32_64 }
    // TODO: Add remaining instantiations
```

```Xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :check
property Contrivance::zero_32 Using exhaustive testing.
Passed 1 tests.
Q.E.D.
property Contrivance::zero_64 Using exhaustive testing.
Passed 1 tests.
Q.E.D.
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_32_64::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^48 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_72::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^72 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_96::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^72 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_96::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^96 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_128::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^96 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_96::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^144 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_144::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^144 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_128::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_192::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_256::EncryptDecryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_32_64::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^48 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_72::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^72 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_96::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^72 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_96::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^96 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_128::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^96 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_96::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^144 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_144::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^144 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_128::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_192::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_256::DecryptEncryptRoundIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_32_64::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^96 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_72::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^120 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_96::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^144 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_96::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^160 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_128::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_96::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_144::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^240 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_128::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^256 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_192::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^320 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_256::EncryptDecryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^384 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_32_64::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^96 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_72::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^120 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_48_96::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^144 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_96::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^160 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_64_128::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_96::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_96_144::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^240 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_128::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^256 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_192::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^320 values)
property labs::NewModuleSystem::NewModuleSystem::SIMON::SIMON_128_256::DecryptEncryptIdentity Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^384 values)
```

## Simon asks why?

Interface, parameterizations, abstractions, and instantiations -- all
to reimplement what parameterized modules are already able to do with
`parameter` blocks and parameterized `import`s. Sure, the new system
allows us to define the whole family of SIMON ciphers, alongside our
earlier contrivance no less, but is this worth all the trouble? If the
answer is immediately obvious at this point, feel free to skip the
rest of this lab, secure in your understanding of nested modules,
interfaces, and the opportunities and conundrums they bring.
Otherwise, let's show off a concept that was difficult to pull off
with the old system: cipher modes.

Recall that our SIMON spec defined `encrypt` and `decrypt` functions
over `key` and `block` sizes. Furthermore, it defined a property that
`decrypt` inverts `encrypt`. These are all rather common, and having
to define the inversion property for AES, Blowfish, RC5, and any other
[block cipher](https://en.wikipedia.org/wiki/Block_cipher)
that comes along gets very tedious very fast.

Now suppose we wish to run each of these ciphers in various
[modes](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation):
[Electronic Codebook (ECB)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#ECB)
(only to demonstrate why that's a really bad idea),
[Cipher Block Chaining (CBC)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CBC),
[Cipher Feedback (CFB)](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#CFB),
... If we had to define each mode, for each block cipher, again, this
would make Cryptol authors wish gone into yodeling or sports
broadcasting. So until this point, except in a few cases where
[an effort was made](https://github.com/GaloisInc/cryptol-specs/tree/master/Primitive/Symmetric/Cipher/Block/Modes),
this has mostly been avoided. Nested modules, for all their verbosity,
offer another approach:
  * Define a block cipher interface
  * Define abstract cipher modes that request block cipher parameters
  * Instantiate cipher modes with parameterized block ciphers

# Block Ciphers

Working from
[NIST SP 800-38A](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication800-38a.pdf),
a _block cipher_ is a function that maps `b`-bit plaintext blocks
to `b`-bit ciphertext blocks, parameterized by a key `K` of arbitrary
type. NIST SP 800-38A refers to block encryption and decryption as
forward and inverse ciphers to disambiguate them from corresponding
mode operations. Let's rename these to `encipher` and `decipher`,
respectively, in keeping with Cryptol naming conventions.

```cryptol
  import labs::Transposition::CommonProperties (inverts)

  interface submodule I_BlockCipher where
    type k : *
    type b : #
    type constraint (fin b)

    encipher : k -> [b] -> [b]
    decipher : k -> [b] -> [b]

  submodule A_BlockCipher_Props where
    import interface submodule I_BlockCipher

    decipher_inverts_encipher : k -> [b] -> Bool
    property decipher_inverts_encipher K =
      inverts (decipher K) (encipher K)
```

Common block cipher _modes of operation_ defined in NIST 800-38A
include:
  * Electronic Codebook (ECB)
  * Cipher Block Chaining (CBC)
  * Cipher Feedback (CFB)
  * Output Feedback (OFB)

Let's start with the easy ones.

```cryptol
  submodule A_ECB where
    import interface submodule I_BlockCipher

    encrypt : {t} k -> [t][b] -> [t][b]
    encrypt K P = map (encipher K) P

    decrypt : {t} k -> [t][b] -> [t][b]
    decrypt K C = map (decipher K) C

    decrypt_inverts_encrypt : {t} (fin t) => k -> [t][b] -> Bool
    decrypt_inverts_encrypt K =
      inverts (decrypt K) (encrypt K)

  submodule A_CBC where
    import interface submodule I_BlockCipher

    encrypt : {t} k -> [b] -> [t][b] -> [t][b]
    encrypt K IV P = C
      where
        C = [ encipher K (P_j ^ C_j') | P_j <- P | C_j' <- [IV] # C ]

    decrypt : {t} k -> [b] -> [t][b] -> [t][b]
    decrypt K IV C = P
      where
        P = [ (decipher K C_j) ^ C_j' | C_j <- C | C_j' <- [IV] # C ]

    decrypt_inverts_encrypt : {t} (fin t) => k -> [b] -> [t][b] -> Bool
    decrypt_inverts_encrypt K IV =
      inverts (decrypt K IV) (encrypt K IV)
```

Unfortunately, because different cipher modes might take different
parameters (e.g. CBC requires an _initialization vector_ `iv`), we
cannot easily define an abstract (sub)module for cipher modes.

**EXERCISE**: Define abstract submodules for CFB and OFB modes.

```cryptol
  submodule A_CFB where
    TODO = undefined

  submodule A_OFB where
    TODO = undefined
```

As tempting as it is to ask students to go reimplement AES, DES, etc.,
let's just adapt an existing cipher to the `I_BlockCipher` interface
expected by our abstract cipher mode submodules. (This part requires
[`cryptol-specs`](https://github.com/GaloisInc/cryptol-specs) to be
cloned or downloaded and in `CRYPTOLPATH`.)

```cryptol
  import module Primitive::Symmetric::Cipher::Block::DES (DES)
  
  submodule P_DES where
    type k = [64]
    type b = 64
    
    encipher = DES.encrypt
    decipher = DES.decrypt

  submodule S_ECB_DES = submodule A_ECB { submodule P_DES }
  import submodule S_ECB_DES as S_ECB_DES

  submodule S_CBC_DES = submodule A_CBC { submodule P_DES }
  import submodule S_CBC_DES as S_CBC_DES
```

Why is ECB bad again?  Let's check...

```cryptol
  ECB_DES_bad : {n} (fin n) => P_DES::k -> [P_DES::b] -> Bool
  ECB_DES_bad K block =
    S_ECB_DES::encrypt K (repeat`{n} block) == repeat (P_DES::encipher K block)

  CBC_DES_less_bad : {n} (fin n, n >= 2) => P_DES::k -> [P_DES::b] -> [P_DES::b] -> Bool
  CBC_DES_less_bad K IV block =
    S_CBC_DES::encrypt K IV (repeat`{n} block) != repeat (P_DES::encipher K block)
```

```Xcryptol-session
labs::NewModuleSystem::NewModuleSystem> :prove ECB_DES_bad`{4}
Q.E.D.
(Total Elapsed Time: 1.893s, using "Yices")
labs::NewModuleSystem::NewModuleSystem> :check CBC_DES_less_bad`{4}
Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^192 values)
```

So ECB is bad unless you never repeat yourself or say anything an
attacker would consider...

**EXERCISE**: Feel free to experiment with other block ciphers
(including SIMON) and modes. Show that ECB is still bad with those.

# Conclusion

In this module, we learned how to apply the new module system to a
previously covered specification (SIMON) and to block cipher modes of
operation by defining common interfaces for abstractions to
instantiate implementations given parameters. The new module system
allowed us to combine all of these and a contrivance into a single
module for expository purposes. In the real world, it will eventually
form a solid foundation to organize a wide variety of cryptographic
algorithms and systems that combine them.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Key Wrapping](../KeyWrapping/KeyWrapping.md) ||
| [< Parameterized Modules](../SimonSpeck/SimonSpeck.md) | **New Module System** ||
|| [! New Module System (Answers)](./NewModuleSystemAnswers.md) ||
