# Learning to use Parameterized Modules with Simon and Speck

## Prerequisites

 * Know how to read Cryptol and skim through a standards document
 * Know how to write Cryptol methods and use basic type parameters in their
   definitions

## What You Will Learn

 * Defining and using Cryptol Parameterized Modules to define families of
   cryptographic routines

## Getting Started

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```shell
cryptol> :m labs::SimonSpeck::SimonSpeck
```
This lab will introduce the student to the use of Cryptol's 
**Parameterized Modules** through the [Simon](https://en.wikipedia.org/wiki/Simon_(cipher)) and [Speck](https://en.wikipedia.org/wiki/Speck_(cipher)) algorithms.

Parameterized Modules allow a user to create a family of modules which differ
by the selection of type parameters that are defined for a base module.

```
module labs::SimonSpeck::SimonSpeck where
```

We will introduce parameterized modules through an implementation of the `Simon`
algorithm. The student will then be asked to imitate this pattern and implement
the `Speck` algorithm.

Note that we have made use of modules throughout the course. In fact each lab
was defined in its own Cryptol module. But because this lab introduces further
concepts around the definition and use of modules, this lab will not be entirely
self-contained in this file.

# Introducing Parameterized Modules

A **parameterized module** is a module which has a collection of type parameters
that can be be used as implicit type variables for all of the definitions it contains. New modules can specify concrete values for these type parameters and easily create a variety of new functionality which reutilizes the functionality in the base parameterized module.

This feature is especially useful for cryptographic applications which often define many variants of a cryptographic routine relying on common functionality. Parameterized modules help specification writers avoid writing redundant code and can help strengthen the confidence in the core of a verified stack of cryptographic primitives.

This template describes how to define a generic parameterized module:

```example
module MyParameterizedModule where

parameter
  
  type param_1 : #
  type param_2 : #
  //...
  type param_k : #

// <<-- Variable and Function Definitions -->>
```
Type parameters `param_1`, `param_2`, *etc...* can be used to declare new types and define variables and functions throughout the module.

New modules can import this module and specify concrete values for these parameters as follows:

```example
module MyConcreteModule = MyParameterizedModule where
  type param_1 = 0
  type param_2 = 1
  //...
  type param_k = 127
```  
Users can then import `MyConcreteModule` which will contain all of the functionality in `MyParameterizedModule` where the functionality is specified to the concrete values.

# Writing a Parameterized Module for Simon

Let's take a look at writing a parameterized module for Simon. We will be using ["The Simon and Speck Families of Lightweight Block Ciphers"](https://eprint.iacr.org/2013/404) as our reference for building this formal specificaiton.

Look at page 10 of this document. We see that the specification defines a family of related block ciphers for `Simon` depending on the following parameters:

  * `word size n` -- The basic register size in bits
  * `key words m` -- The number of words in a key
  * `const seq` -- A bit sequence
  * `rounds T` -- The number of rounds in the key schedule

Furthermore, the different versions of `Simon` are named after the `block size` and the `key size` they use and these two values are functions of the `word size n` and `key words m`:

  * `block size 2*n` -- The size of the block in bits
  * `key size m*n` -- The size of the key in bits

In this lab we have defined a parameterized base module and a family of derived modules in `[CRYPTOLCOURSE]/labs/SimonSpeck/Simon/`. The base module is called `Simon.cry` and the derived modules are named `simon_[bs]_[ks].cry` where `[ks]` and `[bs]` vary overy the valid block and key sized.

Here is the header for the `Simon.cry` base module:

```example
module labs::SimonSpeck::Simon::Simon where

parameter
  
  type n : #    // word size
  type m : #    // words in key
  type T : #    // rounds in the key schedule
  type j : #    // index of z-sequence used (i.e. use sequence z_j)
  type constraint (fin n, fin m, fin T, fin j,
                   16 <= n, n <= 64,
                   2 <= m, m <= 4, 
                   0 <= j, j <= 4, 
                   32 <= T, T <= 72)
type blockSize = 2 * n
type keySize   = m * n

// Other definitions ommitted, see
//
//     [CRYPTOLCOURSE]/labs/SimonSpeck/Simon/Simon.cry
//
// for the remainder of the Simon specificaiton.
```

Note that four module parameters are declared (`n`, `m`, `T`, and `j`) which reflect the four from page 10 of the specification document. We are also able to declare `blockSize` and `keySize` types using these variables that match the definitions in the specification. Note that we use `j` to indicate the index of the sequence `z_j` we will use.

We are also able to specify general type constraints for these parameters. The line that begins `type constraints` places some general bounds on the ranges of values that are possible for each parameter.

We can create a concrete module by specifying values for the module type parameters as follows:

```example
module labs::SimonSpeck::Simon::simon_32_64 = labs::SimonSpeck::Simon::Simon where

  type n = 16
  type m = 4
  type T = 32
  type j = 0
```
This code appears in the file `[CRYPTOLCOURSE]/labs/SimonSpeck/Simon/simon_32_64.cry` and defines a new module `labs::SimonSpeck::Simon::simon_32_64` with concrete values for the `simon32/64` block cipher according to the specification. The user can import this module and use the concretized `encrypt` function as follows:

```
import labs::SimonSpeck::Simon::simon_32_64 as simon_32_64

test_K = join[0x1918, 0x1110, 0x0908, 0x0100]
test_P = join[0x6565, 0x6877]
test_C = join[0xc69b, 0xe9bb]
property test_32_64 = simon_32_64::encrypt test_K test_P == test_C
```

You can check that this test vector passes by running `:check test_32_64`.

# Writing a Parameterized Module for Speck

Now it's your turn to try writing a parameterized module. You will also get some more practice reading and writing a cryptographic specification.

**Exercise** Write a parameterized module for `Speck`. Refer to Section 4 from reference [1 The corresponding `Speck` Cryptol files should be written in the `Speck` folder in this lab directory. You can use the `Simon` implementation as a reference -- many of the patterns you will encounter writing up `Speck` will mirror `Simon`.

If you place the files in the appropriate directory, and name the files/modules as suggested in the `README.md` file, then you can check your work against the test vectors below with the following as follows (with expected output):

```shell
Cryptol> :prove all_speck_vectors_pass 
Q.E.D.
(Total Elapsed Time: 0.021s, using "Z3")
```

**Additional Exercise** The test vectors below only test the encryption direction for `Speck`. Define the decryption direction for the round function and algorihthm and try writing properties to test that these functions are inverses. Example properties can be found in the provided `Simon` specification, you may need to try different solvers like `abc` for some property verifications to complete in a reasonable amount of time.

## Speck Test Vectors

```example
import labs::SimonSpeck::Speck::speck_32_64   as speck_32_64
import labs::SimonSpeck::Speck::speck_48_72   as speck_48_72
import labs::SimonSpeck::Speck::speck_48_96   as speck_48_96
import labs::SimonSpeck::Speck::speck_64_96   as speck_64_96
import labs::SimonSpeck::Speck::speck_64_128  as speck_64_128
import labs::SimonSpeck::Speck::speck_96_96   as speck_96_96
import labs::SimonSpeck::Speck::speck_96_144  as speck_96_144
import labs::SimonSpeck::Speck::speck_128_128 as speck_128_128
import labs::SimonSpeck::Speck::speck_128_192 as speck_128_192
import labs::SimonSpeck::Speck::speck_128_256 as speck_128_256

K_32_64 = join[0x1918, 0x1110, 0x0908, 0x0100]
P_32_64 = join[0x6574, 0x694c]
C_32_64 = join[0xa868, 0x42f2]
property TV_32_64 = speck_32_64::encrypt K_32_64 P_32_64 == C_32_64

K_48_72 = join[0x121110, 0x0a0908, 0x020100]
P_48_72 = join[0x20796c, 0x6c6172]
C_48_72 = join[0xc049a5, 0x385adc]
property TV_48_72 = speck_48_72::encrypt K_48_72 P_48_72 == C_48_72

K_48_96 = join [0x1a1918, 0x121110, 0x0a0908, 0x020100]
P_48_96 = join [0x6d2073, 0x696874]
C_48_96 = join [0x735e10, 0xb6445d]
property TV_48_96 = speck_48_96::encrypt K_48_96 P_48_96 == C_48_96

K_64_96 = join [0x13121110, 0x0b0a0908, 0x03020100]
P_64_96 = join [0x74614620, 0x736e6165]
C_64_96 = join [0x9f7952ec, 0x4175946c]
property TV_64_96 = speck_64_96::encrypt K_64_96 P_64_96 == C_64_96

K_64_128 = join [0x1b1a1918, 0x13121110, 0x0b0a0908, 0x03020100]
P_64_128 = join [0x3b726574, 0x7475432d]
C_64_128 = join [0x8c6fa548, 0x454e028b]
property TV_64_128 = speck_64_128::encrypt K_64_128 P_64_128 == C_64_128

K_96_96 = join [0x0d0c0b0a0908, 0x050403020100]
P_96_96 = join [0x65776f68202c, 0x656761737520]
C_96_96 = join [0x9e4d09ab7178, 0x62bdde8f79aa]
property TV_96_96 = speck_96_96::encrypt K_96_96 P_96_96 == C_96_96

K_96_144 = join [0x151413121110, 0x0d0c0b0a0908, 0x050403020100]
P_96_144 = join [0x656d6974206e, 0x69202c726576]
C_96_144 = join [0x2bf31072228a, 0x7ae440252ee6]
property TV_96_144 = speck_96_144::encrypt K_96_144 P_96_144 == C_96_144

K_128_128 = join [0x0f0e0d0c0b0a0908, 0x0706050403020100]
P_128_128 = join [0x6c61766975716520, 0x7469206564616d20]
C_128_128 = join [0xa65d985179783265, 0x7860fedf5c570d18]
property TV_128_128 = speck_128_128::encrypt K_128_128 P_128_128 == C_128_128

K_128_192 = join [0x1716151413121110, 0x0f0e0d0c0b0a0908, 0x0706050403020100]
P_128_192 = join [0x7261482066656968, 0x43206f7420746e65]
C_128_192 = join [0x1be4cf3a13135566, 0xf9bc185de03c1886]
property TV_128_192 = speck_128_192::encrypt K_128_192 P_128_192 == C_128_192

K_128_256 = join [0x1f1e1d1c1b1a1918, 0x1716151413121110, 
                  0x0f0e0d0c0b0a0908, 0x0706050403020100]
P_128_256 = join [0x65736f6874206e49, 0x202e72656e6f6f70]
C_128_256 = join [0x4109010405c0f53e, 0x4eeeb48d9c188f43]
property TV_128_256 = speck_128_256::encrypt K_128_256 P_128_256 == C_128_256

property all_speck_vectors_pass = and [TV_32_64,  
                                       TV_48_72, TV_48_96,  
                                       TV_64_96, TV_64_128,
                                       TV_96_96, TV_96_144, 
                                       TV_128_128, TV_128_192, TV_128_256]
```

# References

[1] Beaulieu R., Shors D., et. al. "The Simon and Speck Families of Lightweight Block Ciphers". eprint-2013-404.pdf. National Security Agency. June, 2013.


