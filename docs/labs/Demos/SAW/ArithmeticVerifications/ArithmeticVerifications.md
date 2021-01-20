# Introduction

These files are a set of examples meant to demonstrate simple
verifications of arithmetic implemented in C using Cryptol and
SAW. These examples are not meant to be especially deep, but may be
useful as a reference and also to demonstrate some limitations of
these techniques.

# Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * the Software Analysis Workbench (SAW) to be installed.
  
A pre-compiled bitcode file is provided so that you don't need to
compile the sample C code. If you want to compile the C code yourself,
you'll need to install the [Clang C
compiler](https://clang.llvm.org/). **SAW usually lags behind Clang
releases. Check here
(https://github.com/GaloisInc/saw-script#notes-on-llvm) for a list of
supported versions of Clang.**

## Skills You'll Learn

By the end of this lab you will understand how to use SAW to prove
equivalence between Cryptol specifications and simple C functions, and
know some of the limitations of this technique.

You'll also gain experience with the Software Analysis Workbench.

# Getting Started

The file *arithmetic.c* contains definitions for the following functions:

 * `add_standard`

  > __Declaration__: `uint32_t add_standard( uint16_t a, uint16_t b)`
  > __Description__: Adds two 16-bit unsigned integers using built-in addition

 * `add_textbook`
  > __Declaration__: `uint32_t add_textbook( uint16_t a, uint16_t b)`
  > __Description__: Adds two 16-bit integers, but simulates how this might be done on a restricted 8-bit architecture using a "textbook" algorithm

 * `multiply_standard`
  > __Declaration__: `uint32_t multiply_standard( uint16_t a, uint16_t b)`
  > __Description__: Multiplies two 16-bit unsigned integers using built-in multiplication

 * `multiply_textbook`
  > __Declaration__: `uint32_t multiply_textbook( uint16_t a, uint16_t b)`
  > __Description__: Multiplies two 16-bit integers, but simulates how this might be done on a restricted 8-bit architecture using a "textbook" algorithm

 * `multiply_karatsuba`
  > __Declaration__: `uint32_t multiply_karatsuba( uint16_t a, uint16_t b)`
  > __Description__: Multiplies two 16-bit integers, but uses the Karatsuba multiplication technique as though it was binned on 8-bit bins

This set of examples demonstrates how to use Cryptol and SAW to verify that the implementations of these familiar functions are correct and equivalent to one another.

# Walk through

The `Makefile` provided has four targets which drive this set of examples. Running the following commands will drive different portions of this set of examples.

* `make all` (or simply just `make`)

This builds the binary `arithmetic_unit_tests` as well as generates the LLVM bitcode file `arithmetic_llvm.bc`. Running the binary `arithmetic_unit_tests` runs test vectors through the various arithmetic implementations and allows one to check that they are equal on this sample set.

* `make add_tests`

This runs the command `saw add_llvm.saw` which loads `add_standard` and `add_textbook` from the generated LLVM bitcode file `arithmetic_llvm.bc` as well as the additional Cryptol implementation `add_cryptol`. The the script then asks `saw` to prove the following facts:

  - `add_standard` is equivalent to `add_textbook` for all inputs
  - `add_standard` is equivalent to `add_cryptol` for all inputs
  - `add_textbook` is equivalent to `add_cryptol` for all inputs

Note that only two of these facts are necessary to check to demonstrate that all three facts are equivalent. All three are performed for the sake of example. Such redundant checks might be desired to build additional confidence in a system.

* `make multiply_tests`

This step takes a *very* long time to complete. Sample output has been copied below from a sample run. If you are interested, there is some research [1] which justifies why the techniques used by Cryptol and SAW have trouble verifying properties such as these.

This runs the command `saw multiply_llvm.saw` which loads `multiply_standard`, `multiply_textbook`, and `multiply_karatsuba` from the generated LLVM bitcode file `arithmetic_llvm.bc`. Because this takes so long to run, we skip checking against a Cryptol reference as we did for addition. This script checks that:

  - `multiply_standard` is equivalent to `multiply_textbook` for all inputs
  - `multiply_standard` is equivalent to `multiply_karatsuba` for all inputs

which is sufficient to demonstrate that these functions are all equivalent.

Here is the output of a sample run:

    [17:32:52.661] Extracting reference term: multiply_standard
    [17:32:52.696] Extracting implementation term: multiply_textbook
    [17:32:52.699] Extracting implementation term: multiply_karatsuba
    [17:32:52.702] Proving equivalence: multiply_standard == multiply_textbook
    [22:14:04.689] Valid
    [22:14:04.697] Proving equivalence: multiply_standard == multiply_karatsuba
    [04:16:05.144] Valid
    [04:16:05.146] Done.

This output resulted from running the provided script on a relatively modest desktop machine. Note that the first and second equivalence checks required approximately 4.25 and 4 hours respectively.

* `make clean`

This cleans up the workspace and unnecessary generated files.

# References

[[1](https://ieeexplore.ieee.org/document/73590)] Bryant, Randall E. "On the Complexity of VLSI Implementations and Graph Representations of Boolean Functions with Application to Integer Multiplication". Carnegie Mellon University, July 1998.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ SAW Demos ](../Demos.md) ||
|| **Arithmetic Verifications** | [ Bit Twiddling > ](../Bittwiddling/Bittwiddling.md) |

