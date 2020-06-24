[![Build Status](https://travis-ci.com/weaversa/cryptol-course.svg?branch=master)](https://travis-ci.com/weaversa/cryptol-course)

# Programming with Cryptol and SAW

**Purpose:** The purpose of the course is to provide an overview of
the capabilities of [Cryptol](https://github.com/GaloisInc/cryptol)
and the [Software Analysis
Workbench](https://github.com/GaloisInc/saw-script) (SAW). Though,
admittedly, we're a little light on SAW material right now.

-----

## Suggested Lab Order

1. [Installation](INSTALL.md): Get up and running.
2. [Cryptol and SAW Overview](labs/Overview/Overview.md): Learn about
   how Cryptol and SAW are used.
3. [Cryptol Interpreter](labs/Interpreter/Interpreter.md): Learn how
   to use the Cryptol Interpreter.
4. [Language Features](labs/LanguageBasics/LanguageBasics.md): A resource
   for (most) of the language features you'll need to be successful
   here.
    * [Demos](labs/Demos/Demos.md): Lightweight walkthroughs with no
      exercises that demonstrate common concepts.
5. [Cyclic Redundancy Checks](labs/CRC/CRC.md): Create your first
   specification!
6. [Salsa20](labs/Salsa20/Salsa20.md): Create your second
   specification!
7. [Salsa20 Properties](labs/Salsa20/Salsa20Props.md): Learn how to
   prove some properties about your Salsa20 specification.
8. [Prove Cryptographic
   Properties](labs/CryptoProofs/CryptoProofs.md): Learn about common
   cryptographic properties and how to prove them with Cryptol.
    * [Project Euler](labs/ProjectEuler/ProjectEuler.md): If you
      enjoyed the last lab, why not try you hand at using Cryptol's
      connection to automated provers (SMT solvers) to solve some
      complicated computational puzzles.
9. [Methods for Key Wrapping](labs/KeyWrapping/KeyWrapping.md):
   Create a Cryptol specification of NIST's [SP800-38F key wrap
   standard](https://csrc.nist.gov/publications/detail/sp/800-38f/final).
10. [Capstone: Putting it all
   together](labs/LoremIpsum/LoremIpsum.md): Decrypt a series of
   secret messages by feeding wrapping keys into the anomalus KLI20
   cryptographic engine. Success here requires modules and concepts
   from many of the previous labs.


## Graphical View of the Course

Our suggested flow would be to follow the red line. Dependencies are
given by black lines.

<img class="center" src="https://raw.githubusercontent.com/weaversa/cryptol-course/master/misc/deps.svg" alt="Dependencies and Suggested Course Flow">


## Extra Labs and Demonstrations

Some labs or examples go beyond the scope of the course outline, but
may prove interesting or useful for students with wider interests or
different backgrounds. Here are some of them:

1. [Arithmetic Verifications](labs/saw/ArithmeticVerifications/ArithmeticVerifications.md):
   This lab demonstrates using SAW to prove facts about various
   arithmetic implementations. Some simple reference examples are
   included as well as an illustration of a barrier one may experience
   using these techniques.

2. [Bit-twiddling](labs/saw/bittwiddling/bittwiddling.md): This lab contains a
   collection of famous, yet hard to understand algorithms which
   perform relatively simple calculations. This is a good introduction
   to how one can use Cryptol and SAW to prove that an implementation
   matches it's specification.


## How To Take This Course

Many of the labs in this course will be taught using
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
documents --- that is, they can be loaded directly into the Cryptol
interpreter. This README.md is no exception!

Labs have exercises that look like this:

**EXERCISE:** Literate Cryptol documents are meant to be opened up
along side and edited while you work through the labs. For instance,
you might be asked to fill in a portion of a Cryptol snippet:

```comment
algebra_fact : Integer -> Integer -> Bit
property algebra_fact x y =
    //  Use the distributive law to write an equivalent statement
    (x + y) * (x + y) == undefined
```

A student might solve this problem by changing this snippet in their
editor to the following:

```
algebra_fact : Integer -> Integer -> Bit
property algebra_fact x y =
    (x + y) * (x + y) == x^^2 + 2*x*y + y^^2 
```

and then reloading the module using `:reload` (`:r` for short).

Exercises will often have checks that follow along with instructions
for how to verify their work. Check your answer with the following
command; your output should look similar to the following:

```sh
 $ cryptol
 ┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
 ┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
 ┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
 version 2.8.1 (ce0365f)
 
 Loading module Cryptol
 Cryptol> :load README.md
 Loading module Cryptol
 Loading module Main
 Main> :prove algebra_fact 
 Q.E.D.
 (Total Elapsed Time: 0.056s, using Z3)
```

Don't worry -- the first lab walks you through [installing and running
Cryptol](INSTALL.md).


## Supporting Materials

You will find references and supporting materials linked throughout
the course but here are manuals and important documents for easy
reference:

* [Programming
  Cryptol](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf)
  -- A good overview and reference for the Cryptol language. Contains
  many examples and references for programming language features.

* [Cryptol Version 2
  Syntax](https://github.com/GaloisInc/cryptol/blob/master/docs/Syntax.pdf)
  -- A comprehensive guide to Cryptol Syntax

* [Cryptol
  Semantics](https://github.com/GaloisInc/cryptol/blob/master/docs/Semantics.pdf)
  -- A guide to Cryptol language semantics and overview of the
  underlying representations of Cryptol's type system

