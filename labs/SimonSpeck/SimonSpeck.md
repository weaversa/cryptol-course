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

# Implementing Simon with a Parameterized Module

# References

