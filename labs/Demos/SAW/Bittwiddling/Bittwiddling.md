# Introduction

Sean Eron Anderson, a former graduate student at Stanford, collected a
large set of [Bit Twiddling
Hacks](https://graphics.stanford.edu/~seander/bithacks.html). This
collection has a large number of algorithms which perform relatively
simple calculations or test properties (e.g. determine the absolute
value) in surprising ways. The code is often opaque; that is, it's not
easy to see why (or even if) they actually work. This is a good
example of how we can use Cryptol and SAW. With Cryptol, we can write
code that is easy to read, and as such we have high confidence it is
correct. With SAW, we can compare the Cryptol code against the
inscrutable bit twiddling code.

This file will contain your code (written in Cryptol, naturally) that
you use to validate the correctness of the code written in the
corresponding C file [](bittwiddling.c). You should look at the C code
to help you figure out the type signature of your Cryptol
functions. For example, does the C function return a Boolean value
(`True` or `False`) or a number? If a number, how many bits is it?
Your Cryptol code shouldn't match the C code, but instead should be
relatively simple (some are as simple as calling a function that
already exists in Cryptol), and they should all be relatively easy to
read. We also included some properties to help test your code. After
you have completed this code, you should write (from scratch) a
SAWscript to compare the two.

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * the Software Analysis Workbench (SAW) to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in this file.
  
A pre-compiled bitcode file is provided so that you don't need to
compile the sample C code. If you want to compile the C code yourself,
you'll need to install the [Clang C
compiler](https://clang.llvm.org/). **SAW usually lags behind Clang
releases. Check here
(https://github.com/GaloisInc/saw-script#notes-on-llvm) for a list of
supported versions of Clang.**

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * Cryptol's sequence types,
  * manipulating sequences using `#`, `take`, `split`, `join`, `head`,
    `tail`, and `reverse,
  * writing functions and properties,
  * sequence comprehensions,
  * functions with curried parameters,
  * logical, comparison, arithmetic, indexing, slicing, and
    conditional operators, and
  * C programming.

## Skills You'll Learn

By the end of this lab you will gain experience with
  * writing functions and properties, and
  * the Software Analysis Workbench.

## Load This Module

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming)
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::Demos::SAW::Bittwiddling::Bittwiddling
Loading module Cryptol
Loading module labs::Demos::SAW::Bittwiddling::Bittwiddling
```

We start by defining a new module for this lab:

```cryptol
module labs::Demos::SAW::Bittwiddling::Bittwiddling where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

## Parity Exercise

In mathematics, the
[parity](https://en.wikipedia.org/wiki/Parity_(mathematics)) of an
integer simply says whether it is even or odd. For a set of bits, we
extend this definition to finding the parity of the sum of the
bits. For example, the parity `0b0011` is even (the sum of the bits is
2) while `0b1110` is odd (the sum of the bits is 3). Rather than
returning "even" or "odd", we can return "0" if it's even or "1" if
it's odd; since "0" often represents "false" on a computer and "1"
represents "true", the parity function may return one of the Boolean
values.

A straightforward way to calculate the parity of a set of bits is to
XOR all of the bits together: if there's an even number of 1s, the
result is 0, and if there's an odd number of 1s, the result is 1. We
will write a generic parity function and then compare it to three
different lengths: one for a byte (8 bits), one for a word (32 bits),
and one for a double word (64 bits).

### Parity

**EXERCISE**: Write a function that finds the parity of an n-bit
word. Use the three properties below to help you verify that your
function works correctly.

```cryptol
parity : {n} (fin n) => [n] -> Bit
parity w = undefined
```

```cryptol
property parity8bitProp =
    parity 0x01 == True  /\
    parity 0x00 == False /\
    parity 0xff == False /\
    parity 0xef == True
```

```cryptol
property parity32bitProp =
    parity 0x00000001 == True  /\
    parity 0x00000000 == False /\
    parity 0xffffffff == False /\
    parity 0x7fffffff == True
```

```cryptol
property parity64bitProp =
    parity 0x0000000000000001 == True  /\
    parity 0x0000000000000000 == False /\
    parity 0xffffffffffffffff == False /\
    parity 0xffffffff7fffffff == True
```

## Reverse a byte

**EXERCISE**: Write a function which takes a byte and reverses it. For
example, `0b10101010` should map to `0b01010101`. Use the property
below to verify that your function works correctly.

```cryptol
reverseByte : [8] -> [8]
reverseByte b = undefined
```

```cryptol
property reverseByteProp =
    reverseByte 0x01 == 0x80 /\
    reverseByte 0xaa == 0x55
```

## Check if a 32-bit word has a zero byte

**EXERCISE**: Write a function which takes an n*8-bit word and checks
if any one of the bytes is all zeroes. Note that it has to be on a
byte boundary and not just anywhere. For example, `0x11001111` has a
zero byte (the second byte) but `0x10011111` does not (the first byte
is `0x10` while the second byte is `0x01`). Use the property below to
help you verify that your function works correctly.

```cryptol
anyZeroByte : {n} (fin n) => [n*8] -> Bit
anyZeroByte w = undefined
```

```cryptol
property anyZeroByteProp =
    anyZeroByte 0x10011001 == False /\
    anyZeroByte 0x00112233 == True
```

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ SAW Demos](../Demos.md) ||
| [< Arithmetic Verifications](../ArithmeticVerifications/ArithmeticVerifications.md) | **Bit Twiddling** | [Salsa20 (Galois) >](../Salsa20/Salsa20.md) |
|| [! Bit Twiddling (Answers)](./BittwiddlingAnswers.md) ||
