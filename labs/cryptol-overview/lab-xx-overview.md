# Introduction

Cryptol is a domain specific language and tool suite created by Galois, Inc with support from NSA cryptographers. The language has lots of cool programming language features that make it well suited for applications in high assurance systems and cryptographic development, including:

 * strong, static typing
 * type inference
 * parametric size-polymorphism
 * higher-order functions

Cryptol is used to create gold standard algorithm specifications and provides access to tools that facilitate their exploration and evalution.

This lab will provide a quick overview of Cryptol, some motivating applications where the language and technology have been deployed, and to the language features which make Cryptol an excellent choice for these applications.

More information about Cryptol is available at [http://www.cryptol.net](https://cryptol.net).


# Real World Cryptol

Cryptol has been used in the development and evaluation of high assurance cryptographic systems that enjoy wide use. Notable examples where Cryptol has been successfully applied by industry leaders to add assurance to cryptographic implementations include Amazon s2n and Microsoft's Electionguard.

Further examples are distributed with the [Cryptol software source](https://github.com/GaloisInc/cryptol) or are available for review on other projects hosted on [Galois' Github Page](https://github.com/GaloisInc/).


## Amazon s2n Continuous Integration

Amazon s2n is "a C99 implementation of the TLS/SSL protocols that is designed to be simple, small, fast, and with security as a priority". TLS/SSL is a suite of cryptographic protocols and algorithms used to provide integrity, confidentiality and other familiar security services. Amazon s2n is an implementation of this suite used to protect communications on Amazon's cloud infrastructure platforms such as Amazon Web Services (AWS) and Amazon Simple Storage Service (S3).

[TODO: A blurb about cryptol here]

A thorough description of the research, design decisions, and application of Cryptol to evaluating cryptographic implementations in Amazon's s2n system can be found in the paper [Contiuous Formal Verificationof Amazon s2n](https://link.springer.com/chapter/10.1007/978-3-319-96142-2_26). This paper was selected by NSA's Science of Security group for honorable mention in the [7th Annual Best Scientific Cybersecurity Paper Competition](https://cps-vo.org/group/sos/papercompetition/pastcompetitions).

You can review the code for yourself on [Amazon's s2n Github Repository](https://github.com/awslabs/s2n). The code relevant to the specification and evaluation of the HMAC routines can be found in the `tests/saw/` directory.

Further exposition on the development of these integration tests can be found in a three part series on the [Galois Inc. Blog](https://galois.com/blog/): [Part 1](https://galois.com/blog/2016/09/verifying-s2n-hmac-with-saw/): Verifying s2n HMAC with SAW, [Part 2](https://galois.com/blog/2016/09/specifying-hmac-in-cryptol/): Specifying HMAC in Cryptol, and [Part 3](https://galois.com/blog/2016/09/proving-program-equivalence-with-saw/): Proving Program Equivalence with SAW.

## 


# Language Features

So what makes Cryptol so special?

Cryptol is a language designed with Cryptography specifically in mind -- much of the syntax and language was designed with the way that real cryptographers think about and design systems. This allows the Cryptol user to create formal algorithm specifications that closely imitate the style used to describe these algorithms mathematically.

Furthermore, Cryptol provides direct access and easily integrates with powerful evaluation tools such as SAT solvers and the Software Analysis Workbench. These tools allow the user to *prove* facts and demonstrate properties about their code which can provide assurance guarantees that go far beyond simple unit testing.


## Data and Programs

There are five basic data types provided by Cryptol: bits, sequences, integers, tuples, and records.

 * **Bits**: The simplest data type which can take on two values, `True` and `False`.
 * **Sequences**:
 * **Integers**:
 * **Tuples**:
 * **Records**:

## Operators

## Primitives

## Functions

## Functions and Laziness

## Enumerations

## Sequence Comprehensions

## Control Structures

# References
