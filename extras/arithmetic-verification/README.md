Introduction
============

These files are a set of examples meant to demonstrate simple verifications of arithmetic implemented in C using Cryptol and Saw. These examples are not meant to be especially deep, but may be useful as a reference and also to demonstrate some limitations of these techniques.

The file *arithmetic.c* contains definitions for the following functions:
 
 * `add_standard`
 
  > __Declaration__: `uint32_t add_standard( uint16_t a, uint16_t b)`
  > __Description__: Adds two 16-bit unsigned integers using built-in addition
 
 * `add_textbook`
    __Declaration__: `uint32_t add_textbook( uint16_t a, uint16_t b)`
    __Description__: Adds two 16-bit integers, but simulates how this might be done on a restricted 8-bit architecture using a "textbook" algorithm
 
 * `multiply_standard`
  ** __Declaration__: `uint32_t multiply_standard( uint16_t a, uint16_t b)`
  ** __Description__: Multiplies two 16-bit unsigned integers using built-n multiplication 

 * `multiply_textbook`
  * __Declaration__: `uint32_t multiply_textbook( uint16_t a, uint16_t b)`
  * __Description__: Multiplies two 16-bit integers, but simulates how this might be done on a restricted 8-bit architecture using a "textbook" algorithm
 
 * `multiply_karatsuba`
  * __Declaration__: `uint32_t multiply_karatsuba( uint16_t a, uint16_t b)`
  * __Description__: Multiplies two 16-bit integers, but uses the Karatsuba multiplication technique as though it was binned on 8-bit bins

This set of exmples demonstrates how two use Cryptol and Saw to verify that the implementations of these familiar functions are correct and equivalent to one another.


Software Requirements
=====================

This examples requires the following software to operate:

  * clang
  * saw
  * cryptol

These should be configured so that these programs and their components work from your command line.


Walkthrough
===========

The Makefile provided has four targets which drive this set of examples. Running the following commands will drive different portions of this set of examples.

* `make all` (or simply just `make`)

This builds the binary `arithmetic_unit_tests` as well as generates the LLVM bytecode file `arithmetic_llvm.bc`. Running the binary `arithmetic_unit_tests` runs test vectors through the various arithmetic implementations and allows one to check that they are equal on this sample set.

* `make add_tests`

This runs the command `saw add_llvm.saw` which loads `add_standard` and `add_textbook` from the generated LLVM bytecode file `arithmetic_llvm.bc` and which also builds the additional Cryptol function `add_cryptol` which has type `[16] -> [16] -> [32]` and checks the following facts:

  - `add_standard` is equivalent to `add_textbook` for all inputs
  - `add_standard` is equivalent to `add_cryptol` for all inputs
  - `add_textbook` is equivalent to `add_cryptol` for all inputs

Note that only two of these facts are necessary to check to demonstrate that all three facts are equivalent.

* `make multiply_tests`

This step takes a *very* long time to complete. Sample output has been copied below from a sample run. If you are interested, there is some research [1] which justifies why the techniques used by Cryptol and Saw have trouble verifying properties such as these.

This runs the command `saw multiply_llvm.saw` which loads `multiply_standard`, `multiply_textbook`, and `multiply_karatsuba` from the generated LLVM bytecode file `arithmetic_llvm.bc`. Because this takes so long to run, we skip checking against a Cryptol reference as we did for addition. This script checks that:

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

This cleans up the workspace and unnecessary generated files.s


References
==========

[1] Bryant, Randall E. "On the Complexity of VLSI Implementations and Graph Representations of Boolean Functions with Application to Integer Multiplication". Carnegie Mellon University, July 1998.
