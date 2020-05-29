# Programming with Cryptol and SAW

Purpose: The purpose of the course is to provide an overview of the
capabilities of [Cryptol](https://github.com/GaloisInc/cryptol) and the [Software Analysis Workbench](https://github.com/GaloisInc/saw-script) (SAW).

## Potential Flow

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
      - [Caesar Ciphers](specs/Primitive/Symmetric/Cipher/Stream/Caesar.cry): show that encrypt/decrypt is the identity
   - Show examples of how the SAT tool can be used to solve puzzles
      - [Sudoku](specs/Misc/Sudoku.cry): show that solutions to Sudoku puzzles exist and are unique
      - [n-Queens](https://github.com/GaloisInc/cryptol/blob/master/examples/funstuff/NQueens.cry)
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

## Supporting Materials

  * All worked examples from class
  * Additional examples of applications of Cryptol and/or SAW
  * Websites that are relevant for accessing the software
  * Pointers to reference materials & manuals
  * Any other tutorials that can be used to continue learning
