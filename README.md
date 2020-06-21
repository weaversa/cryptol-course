[![Build Status](https://travis-ci.com/weaversa/cryptol-course.svg?branch=master)](https://travis-ci.com/weaversa/cryptol-course)

# Programming with Cryptol and SAW

**Purpose:** The purpose of the course is to provide an overview of
the capabilities of [Cryptol](https://github.com/GaloisInc/cryptol)
and the [Software Analysis
Workbench](https://github.com/GaloisInc/saw-script) (SAW). Though,
admittedly, we're a little light on `SAW` material right now.

Many of the labs in this course will be taught using
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
documents --- that is, they can be loaded directly into the Cryptol
interpreter. This README.md is no exception!

Labs have exercises that look like this:

**EXERCISE:** Literate Cryptol documents are meant to be opened up
along side and edited while you work through the labs. For instance,
you might be asked to fill in a portion of a Cryptol snippet:

```ignore
algebra_fact : Integer -> Integer -> Bit
property algebra_fact x y =
    //  Use the distributive law to write an equivalent statement
    (x + y) * (x + y) == undefined
```

A student might solve this problem by changing this snippet in their
editor as follows:

```
algebra_fact : Integer -> Integer -> Bit
property algebra_fact x y =
    (x + y) * (x + y) == x^^2 + 2*x*y + y^^2 
```

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
 Cryptol> :l README.md
 Loading module Cryptol
 Loading module Main
 Main> :prove algebra_fact 
 Q.E.D.
 (Total Elapsed Time: 0.056s, using Z3)
```

Don't worry -- the first lab walks you through [installing and running
Cryptol](INSTALL.md).

-----

## Suggested Lab Order

1. [Installation](labs/install.md)
   Get up and running.
2. [Introduction](intro.md)
   Learn about Cryptol and SAW.
3. [Demos](labs/Demos/Demos.md)
   Behold the possibilities.
4. [Basics](labs/interpreter/interpreter.md)
   Cut your teeth on the easy stuff.
5. [Salsa20](labs/Salsa20/Salsa20.md)
   Pass an encrypted stream.


## Extra Labs and Demonstrations

Some labs or examples go beyond the scope of the course outline, but
may prove interesting or useful for students with wider interests or
different backgrounds. Here are some of them:

1. Arithmetic Verifications -- Demonstrates using SAW to prove facts
about various arithmetic implementations. Some simple reference
examples are included as well as an illustration of a barrier one may
experience using these techniques.

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

