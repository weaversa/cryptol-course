[![Build Status](https://travis-ci.com/weaversa/cryptol-course.svg?branch=master)](https://travis-ci.com/weaversa/cryptol-course)

# Programming with Cryptol and SAW

**Purpose:** The purpose of the course is to provide an overview of
the capabilities of [Cryptol](https://github.com/GaloisInc/cryptol)
and the [Software Analysis
Workbench](https://github.com/GaloisInc/saw-script) (SAW). Though,
admittedly, we're a little light on SAW material right now.

## How To Take This Course

Many of the labs in this course are taught using
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
documents --- that is, they can be loaded directly into the Cryptol
interpreter. This README.md is no exception! We start by defining a new module for this lab:

```cryptol
module README where
```

Labs have exercises that look like this:

**EXERCISE:** Literate Cryptol documents are meant to be opened up
alongside and edited while you work through the labs. For instance,
you might be asked to fill in a portion of a Cryptol snippet:

```comment
CBCEncrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCEncrypt Ek iv pt = undefined
    //  Implement a parameterized version of the CBC encryption mode

CBCDecrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCDecrypt Dk iv ct = undefined
    //  Implement a parameterized version of the CBC decryption mode
```

A student might solve this problem by changing this snippet in their
editor to the following:

```cryptol
CBCEncrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCEncrypt Ek iv pt =
    [ Ek (pi ^ ci) | pi <- pt | ci <- [iv] # CBCEncrypt Ek iv pt ]

CBCDecrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCDecrypt Dk iv ct =
    [ Dk ci ^ ci' | ci <- ct | ci' <- [iv] # ct ]
```

and then reloading the module using `:reload` (`:r` for short).

Exercises will often have checks that follow along with instructions
for how to verify their work. For example:

```cryptol
property CBCInverts iv (pt : [100][128]) =
    CBCDecrypt (\x -> x - 1) iv (CBCEncrypt (\x -> x + 1) iv pt) == pt
```

Check your answer with the following command; your output should look
similar to the following:

```shell
 ┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
 ┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
 ┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
 version 2.8.0

 Loading module Cryptol
 Cryptol> :module README
 Loading module README
 README> :prove CBCInverts
 Q.E.D.
 (Total Elapsed Time: 0.081s, using Z3)
```

Don't worry if Cryptol is not yet installed on your computer -- the
first lab walks you through [installing and running
Cryptol](INSTALL.md).


## Suggested Lab Order

1. [Installation](INSTALL.md): Get up and running.
2. [Cryptol and SAW Overview](labs/Overview/Overview.md): Learn about
   how Cryptol and SAW are used.
3. [Cryptol Interpreter](labs/Interpreter/Interpreter.md): Learn how
   to use the Cryptol Interpreter.
4. [Language Basics](labs/Language/Basics.md): A resource
   for (most) of the language features you'll need to be successful
   here.
    * [Demos](labs/Demos/Demos.md): Lightweight walkthroughs with no
      exercises that demonstrate common concepts.
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
      enjoyed the last lab, why not try your hand at using Cryptol's
      connection to automated provers (SMT solvers) to solve some
      computational puzzles.
8. [Methods for Key Wrapping](labs/KeyWrapping/KeyWrapping.md):
   Create a Cryptol specification of NIST's [SP800-38F key wrap
   standard](https://csrc.nist.gov/publications/detail/sp/800-38f/final).
9. [Capstone: Putting it all
   together](labs/LoremIpsum/LoremIpsum.md): Decrypt a series of
   secret messages by feeding wrapped keys into the anomalous KLI20
   cryptographic engine. Success here requires use of modules and concepts
   from many of the previous labs.


## Graphical View of the Course

The suggested flow is to follow the red line. The black lines indicate
labs designed to give you more opportunities to practice Cryptol, but
are not strictly necessary for course completion.

<img class="center" src="https://raw.githubusercontent.com/weaversa/cryptol-course/master/misc/deps.svg" alt="Dependencies and Suggested Course Flow">

## Extra Labs and Demonstrations

There are also labs that go beyond the scope of the course outline, but
may prove interesting or useful for students with wider interests or
different backgrounds.

1. [Arithmetic Verifications](labs/saw/ArithmeticVerifications/ArithmeticVerifications.md):
   This lab demonstrates using SAW to prove facts about various
   arithmetic implementations. Some simple reference examples are
   included as well as an illustration of a barrier one may experience
   using these techniques.

2. [Bit-twiddling](labs/saw/bittwiddling/bittwiddling.md): This lab contains a
   collection of famous, yet hard to understand algorithms which
   perform relatively simple calculations. This is a good introduction
   to how one can use Cryptol and SAW to prove that an implementation
   matches its specification.


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
