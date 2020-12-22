[![Build Status](https://travis-ci.com/weaversa/cryptol-course.svg?branch=master)](https://travis-ci.com/weaversa/cryptol-course)

# Programming with Cryptol

**Purpose:** The purpose of the course is to provide an overview of
the capabilities of [Cryptol](https://github.com/GaloisInc/cryptol), a
domain specific language for cryptography. The material can be
undertaken in a self-paced fashion, or is amenable to a more
structured classroom (virtual or physical) presentation and
experimentation environment. The course also briefly touches on the
[Software Analysis Workbench](https://github.com/GaloisInc/saw-script)
(SAW), a related tool for proving properties about software.

## How To Take This Course

This course is composed of a series of labs which introduce aspects of
applications of Cryptol. Many of the labs in this course are taught
using [literate](https://en.wikipedia.org/wiki/Literate_programming)
Cryptol documents --- that is, they render nicely in a web browser or
editor with Markdown support, and they can also be loaded directly
into the Cryptol interpreter. This README.md is no exception! We start
by defining a new module for this file:

```cryptol
module README where
```

Labs have exercises that look like this:

**EXERCISE:** Literate Cryptol documents are meant to be edited while
you work through a lab. For instance, you might be asked to fill in a
portion of a Cryptol snippet:

```comment
CBCEncrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCEncrypt Ek iv pt = undefined
    //  Implement a parameterized version of the CBC encryption mode

CBCDecrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCDecrypt Dk iv ct = undefined
    //  Implement a parameterized version of the CBC decryption mode
```

You might solve this problem by editing the literate document and
changing this snippet to the following:

```cryptol
CBCEncrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCEncrypt Ek iv pt =
    [ Ek (pi ^ ci) | pi <- pt | ci <- [iv] # CBCEncrypt Ek iv pt ]

CBCDecrypt : {n} (fin n) => ([128] -> [128]) -> [128] -> [n][128] -> [n][128]
CBCDecrypt Dk iv ct =
    [ Dk ci ^ ci' | ci <- ct | ci' <- [iv] # ct ]
```

Exercises will often have corresponding properties that you can use to
verify your work. For example:

```cryptol
property CBCInverts iv (pt : [100][128]) =
    CBCDecrypt (\x -> x - 1) iv (CBCEncrypt (\x -> x + 1) iv pt) == pt
```

```Xcryptol session
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.10.0
https://cryptol.net  :? for help

Loading module Cryptol
Cryptol> :module README
Loading module Cryptol
Loading module README
README> :prove CBCInverts
Q.E.D.
(Total Elapsed Time: 0.081s, using "Z3")
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
    * [Style Guide](cryptol-style.md): Cryptol style guide we 
      developed for the course.
    * [Cryptol Demos](labs/Demos/Cryptol/Demos.md): Lightweight
      walkthroughs that demonstrate common Cryptol concepts.
    * [SAW Demos](labs/Demos/SAW/Demos.md): Demonstrations of using
      Cryptol with the Software Analysis Workbench to verify software.
5. [Cyclic Redundancy Checks](labs/CRC/CRC.md): Create your first
   specification.
6. [Salsa20](labs/Salsa20/Salsa20.md): Create your second
   specification.
7. [Prove Cryptographic
   Properties](labs/CryptoProofs/CryptoProofs.md): Learn about common
   cryptographic properties and how to prove them with Cryptol.
    * [Salsa20 Properties](labs/Salsa20/Salsa20Props.md): Prove some
      cryptographic properties about Salsa20.
    * [Transposition Ciphers](labs/Transposition/Contents.md):
      Learn how to use higher-order functions to create and prove
      properties about a number of common transposition ciphers.
    * [Project Euler](labs/ProjectEuler/ProjectEuler.md): If you
      enjoyed the last lab, go ahead and try your hand at using
      Cryptol's connection to automated provers (SMT solvers) to solve
      some classic computational puzzles.
8. [Methods for Key Wrapping](labs/KeyWrapping/KeyWrapping.md):
   Create a Cryptol specification of NIST's [SP800-38F key wrap
   standard](https://csrc.nist.gov/publications/detail/sp/800-38f/final).
    * [Parameterized Modules: Simon and
      Speck](labs/SimonSpeck/SimonSpeck.md): Learn about Cryptol's
      parameterized modules by creating a Cryptol specification of
      NSA's Speck block cipher.
9. [Capstone: Putting it all together](labs/LoremIpsum/LoremIpsum.md):
   Use components and techniques from other labs to decrypt a series
   of secret messages by feeding wrapped keys into the anomalous KLI20
   cryptographic engine.

## Graphical View of the Course

The general course flow is represented by the red lines. The black
lines indicate labs designed to give you more opportunities to
practice Cryptol, but are not strictly necessary for course
completion. (Click on the image below for a navigable representation.)

<img class="center" src="https://raw.githubusercontent.com/weaversa/cryptol-course/L4y3rc4k3/misc/CryptolCourse.gv.svg" alt="Dependencies and Suggested Course Flow">

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

# From here, you can go somewhere!

|Course README| \
[v Installation](INSTALL.md)
