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



# References

