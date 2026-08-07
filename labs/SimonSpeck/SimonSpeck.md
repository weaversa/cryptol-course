# Introduction

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in associated files.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * the `:prove` command,
  * manipulating sequences using `#`, `take`, `split`, `join`,
    `last`, `scanl`, and `transpose`,
  * writing functions and properties,
  * sequence comprehensions,
  * functions with curried parameters, and
  * logical, comparison, and arithmetic operators.

## Skills You'll Learn

By the end of this lab you will have gained experience with defining
and using Cryptol's parameterized modules to make basic families of
cryptographic routines.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::SimonSpeck::SimonSpeck
Loading module Cryptol
Loading module labs::SimonSpeck::Simon::Simon
Loading module labs::SimonSpeck::Simon::simon_32_64
Loading module labs::SimonSpeck::SimonSpeck
```

We start by defining a new module for this lab:

```cryptol
module labs::SimonSpeck::SimonSpeck where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.  
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Learning to use Parameterized Modules with Simon and Speck

This lab will introduce the student to the use of Cryptol's
**Parameterized Modules** through the
[Simon](https://en.wikipedia.org/wiki/Simon_(cipher)) and
[Speck](https://en.wikipedia.org/wiki/Speck_(cipher)) algorithms.

Parameterized Modules allow a user to create a family of modules which
differ by the selection of type parameters that are defined for a base
module.

We will introduce parameterized modules through an implementation of
the `Simon` algorithm. The student will then be asked to imitate this
pattern and implement the `Speck` algorithm.

Note that we have made use of modules throughout the course. In fact
each lab was defined in its own Cryptol module. But because this lab
introduces further concepts around the definition and use of modules,
this lab will not be entirely self-contained in this file.

# Introducing Parameterized Modules

A **parameterized module** is a module which has a collection of type
parameters that can be be used as implicit type variables for all of
the definitions it contains. New modules can specify concrete values
for these type parameters and easily create a variety of new
functionality which reutilizes the functionality in the base
parameterized module.

This feature is especially useful for cryptographic applications which
often define many variants of a cryptographic routine relying on
common functionality. Parameterized modules help specification writers
avoid writing redundant code and can help strengthen the confidence in
the core of a verified stack of cryptographic primitives.

This template describes how to define a generic parameterized module:

```example
module MyParameterizedModule where

parameter
    /** value parameter of built-in type */
    param_builtin : [16][8]

    /** numeric type parameter; must be a finite whole number */
    type param_NumType : #
    type constraint (fin param_NumType, param_NumType >= 1)

    /** arbitrary type parameter; must be comparable and logical */
    type param_AnyType : *
    type constraint (Cmp param_AnyType, Logic param_AnyType)

    /** value parameter of parameter type */
    param_val: param_AnyType

/** a type defined outside parameter scope */
type OtherType = [param_NumType]param_AnyType

/** value parameter defined outside parameter scope */
value_userdef : SomeOtherType

// ...other definitions in public scope; may use parameters defined above...

private
    /** private value of parameter type */
    value_priv_val : param_AnyType
```

Type parameters `param_builtin`, `param_NumType`, *etc...* which
appear indented under the `parameter` keyword declare types which can
be used throughout the module.

We also introduce the `private` keyword here which allows the user to
define functions and values which are only visible from within the
module -- other modules importing this one will not be able to
directly access these definitions.

New modules can import this module and specify concrete values for
these parameters as follows:

```example
module MyConcreteModule = MyParameterizedModule where
    param_builtin = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
                     0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]
    type param_NumType = 42
    type param_AnyType = Bit
    param_val = True
```

Users can then import `MyConcreteModule` which will contain all of the
functionality in `MyParameterizedModule` where the functionality is
specialized to the concrete values.

# Writing a Parameterized Module for Simon

Let's take a look at writing a parameterized module for Simon. We will
be using ["The Simon and Speck Families of Lightweight Block
Ciphers"](https://eprint.iacr.org/2013/404) as our reference for
building this formal specification.

Look at page 10 of this document. We see that the specification
defines a family of related block ciphers for `Simon` depending on the
following parameters:

  * `word size n` -- The basic register size in bits
  * `key words m` -- The number of words in a key
  * `const seq` -- A bit sequence
  * `rounds T` -- The number of rounds in the key schedule

Furthermore, the different versions of `Simon` are named after the
`block size` and the `key size` they use and these two values are
functions of the `word size n` and `key words m`:

  * `block size 2*n` -- The size of the block in bits
  * `key size m*n` -- The size of the key in bits

In this lab we have defined a parameterized base module and a family
of derived modules in `[CRYPTOLCOURSE]/labs/SimonSpeck/Simon/`. The
base module is called `Simon.cry` and the derived modules are named
`simon_[bs]_[ks].cry` where `[ks]` and `[bs]` vary over the valid
block and key sized.

Here is the header for the `Simon.cry` base module:

```example
module labs::SimonSpeck::Simon::Simon where

module labs::SimonSpeck::Simon::Simon where

parameter

  /** word size */
  type n : #
  type constraint (16 <= n, n <= 64)

  /** number of words in key */
  type m : #
  type constraint (2 <= m, m <= 4)

  /** number of rounds in key schedule */
  type T : #
  type constraint (32 <= T, T <= 72)

  /** index of z-sequence used (i.e. use sequence z_j) */
  type j : #
  type constraint (0 <= j, j <= 4)

type blockSize = 2 * n
type keySize   = m * n

// Other definitions omitted, see
//
//     [CRYPTOLCOURSE]/labs/SimonSpeck/Simon/Simon.cry
//
// for the remainder of the Simon specification.
```

Note that four module parameters are declared (`n`, `m`, `T`, and `j`)
which reflect the four from page 10 of the specification document. We
are also able to declare `blockSize` and `keySize` types using these
variables that match the definitions in the specification.

`Simon` defines five different bit sequences `z0, ..., z4`. Note that
we introduce the parameter `j` to indicate the which of these
sequences we will use.

We are also able to specify general type constraints for these
parameters. The line that begins `type constraint` places some
general bounds on the ranges of values that are possible for each
parameter.

We can create a concrete module by specifying values for the module
type parameters as follows:

```example
module labs::SimonSpeck::Simon::simon_32_64 = labs::SimonSpeck::Simon::Simon where

  type n = 16
  type m = 4
  type T = 32
  type j = 0
```

This code appears in the file
`[CRYPTOLCOURSE]/labs/SimonSpeck/Simon/simon_32_64.cry` and defines a
new module `labs::SimonSpeck::Simon::simon_32_64` with concrete values
for the `simon32/64` block cipher according to the specification. The
user can import this module and use the concretized `encrypt` function
as follows:

```
import labs::SimonSpeck::Simon::simon_32_64 as simon_32_64

test_K = join [0x1918, 0x1110, 0x0908, 0x0100]
test_P = join [0x6565, 0x6877]
test_C = join [0xc69b, 0xe9bb]
property test_32_64 = simon_32_64::encrypt test_K test_P == test_C
```

You can check that this test vector passes by running `:check
test_32_64`.

## Simon Details

We will not walk through the entire implementation of Simon; however,
we will describe some of the definitions and interesting features
found in the specification.

Additional documentation can be found in the Simon source file
`[CRYPTOLCOURSE]/labs/SimonSpeck/Simon/Simon.cry` which you are
encouraged to review as part of this lab.

**Bit Sequences** `Simon` defines five different bit sequences and
each `Simon` variant selects one of these to use in its definition. We
could have defined a value parameter which could have been specified
in each of the concrete `Simon` modules; however, the bit sequences
are reused between these variants.

To avoid having to rewrite the bit sequences we define all five in the
base parameterized module as follows:

```example
    // We define the sequences u, v, w, and z_j following the paper
    u = join (repeat`{inf} 0b1111101000100101011000011100110)
    v = join (repeat`{inf} 0b1000111011111001001100001011010)
    w = join (repeat`{inf} 0b1000010010110011111000110111010)

    z0 = u
    z1 = v
    z2 = join [ dibit ^ 0b01 | dibit <- groupBy`{2} u ]
    z3 = join [ dibit ^ 0b01 | dibit <- groupBy`{2} v ]
    z4 = join [ dibit ^ 0b01 | dibit <- groupBy`{2} w ]

    // Z will be the sequence selected by the module parameter j
    Z = [z0, z1, z2, z3, z4]@(`j:[width j])
```

The module parameter `j` is then used to index into the array of these
bit sequences `[z0, z1, z2, z3, z4]` to select a specific sequence for
each variant.  The variable `Z` then refers to the selected sequence
and is used in the definition of the round function.

**Key Expansion**

There are two interesting features of the `KeyExpansion` function in
this implementation which lead, ultimately, to conditionally defined
behavior when the number of `Simon` key words `m` is equal to `4`.

Here is the code that we will be discussing:

```example
    /**
     * "tmp" appears as the name of a variable in the Simon key
     * expansion sample implementation on page 13. The tmp function
     * below captures the possible final values the expression has,
     * taking into account the type parameter `m` containing the
     * number of key words.
     */
    tmp: [n] -> [n] -> [n]
    tmp k1 k2 = r
      where 
        r  = t ^ (t >>> 1)
        t  = if `m == 0x4 then (t' ^ k2) else t'
        t' = (k1 >>> 3)

    /**
     * The Simon Key Expansion function.
     */
    KeyExpansion : [keySize] -> [T][n]
    KeyExpansion K = take Ks
      where
        Kis : [m][n]
        Kis = reverse(split K)
        Ks : [inf][n]
        Ks = Kis # [ ~k0 ^ (tmp k1 k2) ^ (zext [z]) ^ (zext 0x3) 
                   | k0 <- Ks
                   | k1 <- drop`{m-1} Ks
                   | k2 <- drop`{max m 3  - 3} Ks // gadget to typecheck "drop`{m-3}"
                   | z <- Z ]
```

First, the `tmp` function is named to mirror the sample implementation
found in the specification on page 13. `KeyExpansion` xors in earlier
values of the key expansion conditionally on whether there are four
key words (`m == 4`) for this variant.

Note that when `m != 4` the second parameter `k2` is ignored by the
conditional statement defining `t`.

Second, in the parallel enumeration found in `KeyExpansion` we see
that `k2` is drawn from the sequence ``drop`{max m 3 - 3}``. The
`max m 3` expression guarantees that after subtracting `3` the result
is non-negative. Note that the value `k2` is ignored in the `tmp`
function when `m != 4` and that `max m 3 - 3` is equivalent to `m - 3`
when `m == 4`.

Without the `max m 3` expression, Cryptol's type checker will detect
that the `m - 3` could potentially be negative and suggest that an
additional constraint `m >= 3` should be added. However, we want to
allow `m` to take on the values `2` or `3` as well, hence add this as
a workaround.

Ultimately, these choices allow us to define a single `KeyExpansion`
function for all variants of `Simon`.

# Writing a Parameterized Module for Speck

Now it's your turn to try writing a parameterized module. You will
also get more practice reading and writing a cryptographic
specification.

**EXERCISE**: Write a parameterized module for `Speck`. Refer to
Section 4 from reference [1]. The corresponding `Speck` Cryptol files
should be written in the `Speck` folder provided in this lab
directory. Consider using the `Simon` implementation as a reference --
many of the patterns you will encounter writing up `Speck` will mirror
`Simon`.

If you name your files as suggested in the [Speck
README.md](./Speck/README.md) file, then you should be
able to load the `SpeckTestVectors` module also located in the `Speck`
folder as follows and verify that you have correctly implemented the
functions:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::SimonSpeck::Speck::SpeckTestVectors
labs::SimonSpeck::Simon::SpeckTestVectors> :prove all_speck_vectors_pass 
Q.E.D.
(Total Elapsed Time: 0.021s, using "Z3")
```

**ADDITIONAL EXERCISE**: The test vectors below only test the
encryption direction for `Speck`. Define the decryption direction for
the round function and algorithm and try writing properties and/or
test vectors to verify that these functions are inverses. You may need
to try different solvers like `abc` for some property verifications to
complete in a reasonable amount of time.

# References

[1] Beaulieu R., Shors D., et. al. "The Simon and Speck Families of Lightweight Block Ciphers". eprint-2013-404.pdf. National Security Agency. June, 2013.

[2] Beaulieu R., Shors D., et. al. "SIMON and SPECK Implementation Guide".
National Security Agency. January, 2019.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [^ Module System](../ModuleSystem.md) ||
|| **Parameterized Modules** | [New Module System >](../NewModuleSystem/NewModuleSystem.md) |
