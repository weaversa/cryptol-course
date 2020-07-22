[![Build Status](https://travis-ci.com/weaversa/cryptol-course.svg?branch=master)](https://travis-ci.com/weaversa/cryptol-course)

# Programming with Cryptol and SAW

**Purpose:** This course provides an introduction to the
[Cryptol](https://github.com/GaloisInc/cryptol) language
(and to a lesser extent the [Software Analysis Workbench
(SAW)](https://github.com/GaloisInc/saw-script)) through description,
exposition, and hands-on development and experimentation.  The
material can be undertaken in a self-paced fashion, or is amenable to
a more structured classroom (virtual or physical) presentation and
experimentation environment.

## Course Components

The course is composed of a series of modules which introduce aspects
or applications of Cryptol.  Some of the modules build on concepts and
elements from other portions; these relationships are reflected in the
"Prerequisites" section of each module, and are shown graphically in
the diagram below.

1. [Installation](INSTALL.md): Get up and running.
2. [Cryptol and SAW Overview](labs/Overview/Overview.md): Learn about
   how Cryptol and SAW are used.
3. [Cryptol Interpreter](labs/Interpreter/Interpreter.md): Learn how
   to use the Cryptol Interpreter.
4. [Language Basics](labs/Language/Basics.md): A resource
   for (most) of the language features you'll need to be successful
   here.
    * [Demos](labs/Demos/Demos.md): Lightweight walkthroughs that
      demonstrate common concepts.
5. [Cyclic Redundancy Checks](labs/CRC/CRC.md): Create your first
   specification.
6. [Salsa20](labs/Salsa20/Salsa20.md): Create your second
   specification.
7. [Prove Cryptographic
   Properties](labs/CryptoProofs/CryptoProofs.md): Learn about common
   cryptographic properties and how to prove them with Cryptol.
    * [Salsa20 Properties](labs/Salsa20/Salsa20Props.md): Prove some
      cryptographic properties about Salsa20.
    * [Project Euler](labs/ProjectEuler/ProjectEuler.md): If you
      enjoyed the last lab, go ahead and try your hand at using Cryptol's
      connection to automated provers (SMT solvers) to solve some
      classic computational puzzles.
8. [Methods for Key Wrapping](labs/KeyWrapping/KeyWrapping.md):
   Create a Cryptol specification of NIST's [SP800-38F key wrap
   standard](https://csrc.nist.gov/publications/detail/sp/800-38f/final).
9. [Capstone](labs/LoremIpsum/LoremIpsum.md): Use components and
   techniques from other labs to decrypt a series of secret messages
   by feeding wrapped keys into the anomalous KLI20 cryptographic
   engine. 

### Extra Labs and Demonstrations

There are also modules that go beyond the scope of the course outline but
may prove interesting or useful for students with wider interests or
different backgrounds.

1. [Arithmetic
   Verifications](labs/saw/ArithmeticVerifications/ArithmeticVerifications.md):
   This lab demonstrates using SAW to prove facts about various
   arithmetic implementations. Some simple reference examples are
   included as well as an illustration of a barrier one may encounter
   when using these techniques.

2. [Bit-twiddling](labs/saw/bittwiddling/bittwiddling.md): This lab
   contains a collection of famous, yet hard to understand algorithms
   which perform relatively simple calculations. This is an
   introduction to how one can use Cryptol and SAW to prove that an
   implementation matches its specification.

The general course flow is represented by the red line. The black lines
indicate modules designed to give you more opportunities to practice
Cryptol, but are not strictly necessary for course completion.  (Click
on the image below for a navigable representation.)

<img class="center"
src="https://raw.githubusercontent.com/weaversa/cryptol-course/master/misc/deps.svg"
alt="Dependencies and Suggested Course Flow">

## How To Take This Course

This modules of this course are monolithic
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
documents with a `.md` suffix: each file contains both exposition
about the topic at hand as well embedded lab exercises.  When viewed
via a web browser (either on the [GitHub cryptol
course](https://github.com/saweave/cryptol-course) site or via a local
mechanism), the file will be rendered like this:

<hr>
...

### Functions

Functions in Cryptol can include input parameters, perform operations
on those parameters, and generate a result.  

**EXERCISE:** Create the body of the function below.

```comment
absValue : Integer -> Integer
absValue n = undefined
    // Replace 'undefined' with code to determine the absolute value
```
...
<hr>

To complete the exercise, the student can edit the literate `.md` file
and load it directly into the Cryptol interpreter (either as a startup
parameter to Cryptol or via the `:load` command).  Cryptol will
interpret the code blocks (while ignoring the other text) and enable
the student to interact with the environment.  (More detail on this
process is in the [Cryptol
Interpreter](labs/Interpreter/Interpreter.md) module.)  In this case,
a student might solve this problem by editing the literate file and
changing this snippet to the following:

```cryptol
absValue : Integer -> Integer
absValue n = if n >= 0 then n else -n
```

then loading the module and testing their solution.

<hr>

```
 $ cryptol
 ┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
 ┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
 ┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
 version 2.8.0

 Loading module Cryptol
 Cryptol> :load README.md
 Loading module Cryptol
 Loading module Main
 Main> absValue 5
 5
 Main> absValue (-6)
 6
```

<hr>

Don't worry if Cryptol is not yet on your system -- the first lab
walks you through [installing and running Cryptol](INSTALL.md).

## Supporting Materials

You will find references and supporting materials linked throughout
the course, but here are some key manuals and documents for easy
reference:

* [Programming
  Cryptol](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf)
  -- A comprehensive reference for the Cryptol language. Contains many
  examples for programming language features including a full workup
  of AES.

* [Cryptol
  Syntax](https://github.com/GaloisInc/cryptol/blob/master/docs/Syntax.pdf)
  -- A comprehensive guide to Cryptol Syntax.

* [Cryptol
  Primitives](https://github.com/GaloisInc/cryptol/blob/master/docs/CryptolPrims.pdf)
  -- A simple list of all of the Cryptol language primitives.

* [Cryptol Module System
  Overview](https://github.com/GaloisInc/cryptol/blob/master/docs/AbstractValuesAndModuleParameters.md)
  -- An overview of the Cryptol's parameterized module system. This
  feature assists in creating parameterized algorithms such as AES
  (which has three different variants with different key sizes,
  namely, 128, 192, and 256).
