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

A *parameterized module* is a module which has a collection of type parameters
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

// <-- Variable and Function Definition
```

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
                   2 <= m, m <= 4, 
                   0 <= j, j <= 4, 
                   m <= T-1, T >= 22, 
                   n >= width T)
type blockSize = 2 * n
type keySize   = m * n

// Other definitions ommitted, see
//     [CRYPTOLCOURSE]/labs/SimonSpeck/Simon/Simon.cry
```

Note that four module parameters are declared

# Writing a Parameterized Module for Speck

Now it's your turn to try writing a parameterized module.

**Exercise** Write a parameterized module for Speck. This should be written in the `Speck` folder in this lab directory.

# References

[1] Beaulieu R., Shors D., et. al. "The Simon and Speck Families of Lightweight Block Ciphers". eprint-2013-404.pdf. National Security Agency. June, 2013.


