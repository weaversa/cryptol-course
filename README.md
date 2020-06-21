[![Build Status](https://travis-ci.com/weaversa/cryptol-course.svg?branch=master)](https://travis-ci.com/weaversa/cryptol-course)

# Programming with Cryptol and SAW

**Purpose:** The purpose of the course is to provide an overview of
the capabilities of [Cryptol](https://github.com/GaloisInc/cryptol)
and the [Software Analysis
Workbench](https://github.com/GaloisInc/saw-script) (SAW).

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

Don't worry -- we also walk you through [accessing and installing
Cryptol](INSTALL.md).

## Course Outline

1. Description of Cryptol and SAW
2. Demonstrate example of a cryptographic primitive with Cryptol (and SAW?)
   - Possibly the packaged [Salsa20 example](https://github.com/GaloisInc/saw-script/tree/master/examples/salsa20)
   - Possibly simple examples (XOR, Caesar)
   - Possibly common real-world algorithms (AES, SHA, ...)
3. Show the ability of Cryptol / SAW to do [equivalence checking](https://en.wikipedia.org/wiki/Formal_equivalence_checking)
   - Demonstrate that it can do unit tests as well
   - Explain about how checking properties is strictly stronger
   - Demonstrate the ease of comparing good Cryptol code with a specification
   - Demonstrate that some problems are unsuitable for SAT solving
     (e.g. finding cipher key, verifying optimized multiplication)
4. Get students up and running
   - Verify (as well as possible) everyone can run the software for the course
   - Run the demo just worked themselves
5. Building familiarity with Cryptol
   - Students interact and experiment with partially worked examples
   - Verify some properties of simple functions
      - [XOR](https://github.com/GaloisInc/cryptol/blob/master/examples/xor_cipher.cry): show that double xor is the identity
      - [Caesar Ciphers](labs/Demos/Caesar.md): show that encrypt/decrypt is the identity
   - Show examples of how the SAT tool can be used to solve puzzles
      - [Sudoku](labs/Demos/Sudoku.md): show that solutions to Sudoku puzzles exist and are unique
      - [n-Queens](labs/Demos/NQueens.md): show that queens don't see each other
6. Programming with Cryptol, Round 1
   - Present material on some of the Cryptol language basics, students have seen
     some of this now in context and have examples to pull from
   - Have students write a variety of simple functions exercising different
     elements of Cryptol syntax -- there's a lot of this sort of thing in
     Galois' programming guide to pull from
   - Presenters walk around and assist as necessary
7. Programming with Cryptol, Round 2
   - Have students try to work through a more complex cryptographic exercise
8. Verifying with SAW
   - Possibly have students follow a script to verify a provided example
   - Demonstrate how code from different languages can be brought together and
     properties checked
9. Introduction to advanced (new) tools
   - [crux](https://github.com/GaloisInc/crucible)
   - [argo](https://github.com/GaloisInc/argo)

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

