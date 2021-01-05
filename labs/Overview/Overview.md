# Introduction

This lab will provide a quick overview of Cryptol, some motivating
applications where the language and technology have been deployed, and
to the language features which make Cryptol an excellent choice for
these applications.

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed and
  * this module to load successfully.

## Skills You'll Learn

By the end of this lab you will have gained a basic understanding of
the utility of Cryptol and some of the tools that interact with
Cryptol (such as the Software Analysis Workbench).

You'll also gain experience with
  * loading modules, evaluating functions, and browsing symbols in the
    interpreter,
  * Cryptol's `Bit`, sequence, `Integer`, tuple, and record types,
  * the `:prove` command,
  * manipulating sequences using `take`, `drop`, `split`, `groupBy`,
    `sum`, `min`, `max`, `tail`, `last`, and `foldl`,
  * lambda functions,
  * enumerations and sequence comprehensions,
  * `/\`, `\/`, `==>` -- single bit logical operations,
  * `~`, `&&`, `||`, `^` -- logical operations for sequences,
  * `==`, `!=` -- structural comparison,
  * `==`, `>=`, `>`, `<=`, `<` -- nonnegative word comparisons,
  * `+`, `-`, `*`, `/`, `%`, `**` -- wordwise modular arithmetic,
  * `>>`, `<<`, `>>>`, `<<<` -- shifts and rotates,
  * `#` -- concatenation,
  * `@` -- sequence indexing, and
  * `if then else` -- conditional expressions.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::Overview::Overview
Loading module Cryptol
Loading module labs::Overview::Overview
```

We start by defining a new module for this lab:

```cryptol
module labs::Overview::Overview where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Overview of Cryptol

Cryptol is a domain specific language and tool suite created by
Galois, Inc., with support from NSA cryptographers. The language has
lots of cool programming language features that make it well suited
for applications in high assurance systems and cryptographic
development, including:

 * strong, static typing
 * type inference
 * parametric size-polymorphism
 * higher-order functions

Cryptol is used to create gold-standard algorithm specifications and
provides access to tools that facilitate their exploration and
evaluation. More information about Cryptol is available at
[http://www.cryptol.net](https://cryptol.net).

# First Steps: Hello, Cryptol!

A grand tradition when learning a new programming language is to test
the waters with a Hello World program. In this file we've defined a
function `sayHello` which is specified as follows:

void sayHello (uint8_t *name, size_t a)

```cryptol
sayHello : {a} (fin a) => [a][8] -> [7+a][8]
sayHello name = greeting
  where
    greeting = "Hello, " # name
```

You can then run this function by typing the following into the
Cryptol interpreter.

```Xcryptol-session
labs::Overview::Overview> sayHello "Cryptol!"
[0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x43, 0x72, 0x79, 0x70,
 0x74, 0x6f, 0x6c, 0x21]
labs::Overview::Overview> :set ascii=on
labs::Overview::Overview> sayHello "Cryptol!"
"Hello, Cryptol!"
labs::Overview::Overview>
```
Congratulations, you are now officially on speaking terms!

# Cryptol in the Wild

Cryptol has been used in the development and evaluation of high assurance cryptographic systems that enjoy wide use. Notable examples where Cryptol has been successfully applied by industry leaders to add assurance to cryptographic implementations include Amazon s2n and Microsoft's ElectionGuard.

Further examples are distributed with the [Cryptol software source](https://github.com/GaloisInc/cryptol) or are available for review on other projects hosted on the [Galois Github Page](https://github.com/GaloisInc/).


## Amazon s2n Continuous Integration

[Amazon s2n](https://aws.amazon.com/blogs/security/introducing-s2n-a-new-open-source-tls-implementation/) is "a C99 implementation of the TLS/SSL protocols that is designed to be simple, small, fast, and with security as a priority". TLS/SSL is a suite of cryptographic protocols and algorithms used to provide integrity, confidentiality and other familiar security services. Amazon s2n is an implementation of this suite used to protect communications on Amazon's cloud infrastructure platforms such as Amazon Web Services (AWS) and Amazon Simple Storage Service (S3).

These security property tests are performed as part of a continuous integration pipeline using the [Travis Continuous Integration Service](https://travis-ci.com/). Whenever changes are made -- no matter how small -- to the C implementations, Cryptol and SAW evaluations are automatically run to ensure that no security properties of the system have been disrupted by the proposed updates.

A thorough description of the research, design decisions, and application of Cryptol to evaluating cryptographic implementations in Amazon's s2n system can be found in the paper [Continuous Formal Verification of Amazon s2n](https://link.springer.com/chapter/10.1007/978-3-319-96142-2_26). This paper was selected by NSA's Science of Security group for honorable mention in the [7th Annual Best Scientific Cybersecurity Paper Competition](https://cps-vo.org/group/sos/papercompetition/pastcompetitions).

You can review the code for yourself on [Amazon's s2n Github Repository](https://github.com/awslabs/s2n). The code relevant to the specification and evaluation of the HMAC routines can be found in the `tests/saw/` directory.

Further exposition on the development of these integration tests can be found in a three part series on the [Galois Inc. Blog](https://galois.com/blog/):
 * [Part 1](https://galois.com/blog/2016/09/verifying-s2n-hmac-with-saw/) - **Verifying s2n HMAC with SAW**
 * [Part 2](https://galois.com/blog/2016/09/specifying-hmac-in-cryptol/) - **Specifying HMAC in Cryptol**
 * [Part 3](https://galois.com/blog/2016/09/proving-program-equivalence-with-saw/) - **Proving Program Equivalence with SAW**

## Verifying Cryptographic Implementations: The `xxhash` Algorithm

The tools that Cryptol provides access to allows users to bring together cryptographic implementations from other languages like *Java* or *C* and prove that they are equivalent to "gold standard" specifications one can create in Cryptol. This allows users to iteratively optimize code in performance-centric or system languages while maintaining a single trusted specification in Cryptol.

Take a look through the [`saw-demos` repository](https://github.com/GaloisInc/saw-demos) by GaloisInc hosted on GitHub which highlights several of these applications. We will do a brief survey of the `xxhash` example which you can find in the `demos/xxhash` directory of this repository. This is a demo of using Cryptol algorithm specifications (along with SAW).

This directory contains the following files:

```Xcryptol-session
.
├── Makefile
├── output
├── xxhash32-ref.c
├── xxhash32-ref.saw
├── xxhash64-ref.c
├── xxhash64-ref.saw
└── xxhash.cry
```

[`xxhash.cry`](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash.cry) contains Cryptol specifications for 32- and 64-bit variants of the `xxhash` algorithm along with related subroutines. The 32-bit variant has the following definition:

```haskell
XXH32 : {L} (fin L) => [L][8] -> [32] -> [32]
XXH32 input seed = XXH32_avalanche acc1
  where (stripes16 # stripes4 # stripes1) = input
        accR = foldl XXH32_rounds (XXH32_init seed) (split stripes16 : [L/16][16][8])
        accL = `(L % 2^^32) + if (`L:Integer) < 16
                              then seed + PRIME32_5
                              else XXH32_converge accR
        acc4 = foldl XXH32_digest4 accL (split stripes4 : [(L%16)/4][4][8])
        acc1 = foldl XXH32_digest1 acc4 (stripes1 : [L%4][8])
```

This function depends on other components defined in this file, such as `XXH32_avalanche`, `XXH32_rounds`, and `XXH32_init` which you can take a look at as well. At a glance, we see that this function has the type signature `{L} (fin L) => [L][8] -> [32] -> [32]` which indicates that this function takes a finite sequence of bytes and a 32-bit seed, and produces a 32-bit result (the hash).

[`xxhash32-ref.c`](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash32-ref.saw) and [`xxhash64-ref.c`](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash64-ref.c) contain `C` implementations of the `xxhash` algorithm which might commonly be seen in a real-world system implementation where performance was critical. Here is a snippet containing the `C` implementation of the hash function called`XXH32`:

```C
/* The XXH32 hash function.
 * input:   The data to hash.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * seed:    A 32-bit value to seed the hash with.
 * returns: The 32-bit calculated hash value. */
uint32_t XXH32(void const *const input, size_t const length, uint32_t const seed)
{
    uint8_t const *const data = (uint8_t const *) input;
    uint32_t hash;
    size_t remaining = length;
    size_t offset = 0;

    /* Don't dereference a null pointer. The reference implementation notably doesn't
     * check for this by default. */
    if (input == NULL) {
        return XXH32_avalanche(seed + PRIME32_5);
    }

    if (remaining >= 16) {
        /* Initialize our accumulators */
        uint32_t acc1 = seed + PRIME32_1 + PRIME32_2;
        uint32_t acc2 = seed + PRIME32_2;
        uint32_t acc3 = seed + 0;
        uint32_t acc4 = seed - PRIME32_1;

        while (remaining >= 16) {
            acc1 = XXH32_round(acc1, XXH_read32(data, offset)); offset += 4;
            acc2 = XXH32_round(acc2, XXH_read32(data, offset)); offset += 4;
            acc3 = XXH32_round(acc3, XXH_read32(data, offset)); offset += 4;
            acc4 = XXH32_round(acc4, XXH_read32(data, offset)); offset += 4;
            remaining -= 16;
        }

        hash = XXH_rotl32(acc1, 1) + XXH_rotl32(acc2, 7) + XXH_rotl32(acc3, 12) + XXH_rotl32(acc4, 18);
    } else {
        /* Not enough data for the main loop, put something in there instead. */
        hash = seed + PRIME32_5;
    }

    hash += (uint32_t) length;

    /* Process the remaining data. */
    while (remaining >= 4) {
        hash += XXH_read32(data, offset) * PRIME32_3;
        hash  = XXH_rotl32(hash, 17);
        hash *= PRIME32_4;
        offset += 4;
        remaining -= 4;
    }

    while (remaining != 0) {
        hash += (uint32_t) data[offset] * PRIME32_5;
        hash  = XXH_rotl32(hash, 11);
        hash *= PRIME32_1;
        --remaining;
        ++offset;
    }
    return XXH32_avalanche(hash);
}
```
Finally the files [`xxhash32-ref.saw`](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash32-ref.saw) and [`xxhash64-ref.saw`](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash64-ref.saw) contain SAW scripts which drive the verification that this `C` code is equivalent to the specification found in this Cryptol specification of `xxhash`.

Running `make` at the commandline will initiate the verification for both the 32- and 64-bit implementations, producing the following output:

```Xcryptol-session
$ make
clang xxhash32-ref.c -o xxhash32-ref.bc -c -emit-llvm -O0 -std=c90
clang xxhash64-ref.c -o xxhash64-ref.bc -c -emit-llvm -O0 -std=c90
saw xxhash32-ref.saw
[17:48:43.283] Loading file "/home/user/Projects/saw-demos/demos/xxhash/xxhash32-ref.saw"
[17:48:43.285] Loading file "/home/user/Projects/saw-demos/common/llvm.saw"
[17:48:43.613] Verifying XXH_rotl32 ...
[17:48:43.615] Simulating XXH_rotl32 ...
[17:48:43.617] Checking proof obligations XXH_rotl32 ...
[17:48:43.778] Proof succeeded! XXH_rotl32

... output omitted ...

[17:48:54.220] Checking proof obligations XXH64 ...
[17:48:54.311] Proof succeeded! XXH64
```

These scripts will check that the `C` implementations match the Cryptol specification for *every possible* input for the hash lengths specified. This is important to highlight because this is far beyond the capability of unit testing to detect errors. For instance, for inputs of length `128` bits, there are 2<sup>160</sup> input/seed combinations to check. Unit tests -- even random unit tests -- may only typically cover a few hundred or thousand cases. Cryptol and SAW are able to provide confidence on a space many orders of magnitude larger.

## Verifying Properties about Algorithms

Cryptol provides an easy interface for using powerful tools such as SMT solvers for verifying properties about algorithms we care about. Throughout this course, we will introduce examples and explain how to take advantage of these tools in your own designs and evaluations. Here is an example packaged with the Cryptol source that demonstrates a simple but important property about an encryption algorithm which only uses the (XOR) operation:

```cryptol
encrypt : {a}(fin a) => [8] -> [a][8] -> [a][8]
encrypt key plaintext = [ pt ^ key | pt <- plaintext ]

decrypt : {a}(fin a) => [8] -> [a][8] -> [a][8]
decrypt key ciphertext = [ ct ^ key | ct <- ciphertext ]

property roundtrip key plaintext = decrypt key (encrypt key plaintext) == plaintext
```

This file defines an `encrypt` operation, a `decrypt` operation, and a property called `roundtrip` which checks for all keys `key` and all input plaintexts `plaintext` that `decrypt key (encrypt key plaintext) == plaintext` (*i.e.* that these operations are the inverse of each other).

We can see the effect of encrypting the particular input `attack at dawn` with the key `0xff`:

```Xcryptol-session
labs::Overview::Overview> :s ascii=on
labs::Overview::Overview> encrypt 0xff "attack at dawn"
"\158\139\139\158\156\148\223\158\139\223\155\158\136\145"
labs::Overview::Overview> decrypt 0xff it
"attack at dawn"
```

Cryptol interprets the string `"attack at dawn"` as a sequence of bytes suitable for the encrypt operations. (We will introduce Cryptol types later in this lab and discuss them in detail throughout this course.)

We can prove the `roundtrip` property holds in the interpreter using the `:prove` command and the currently configured SMT solver (Z3 by default):

```Xcryptol-session
labs::Overview::Overview> :prove roundtrip : [8] -> [16][8] -> Bit
Q.E.D.
(Total Elapsed Time: 0.010s, using "Z3")
```

Cryptol reports `Q.E.D.`, indicating that our property is indeed true for all keys and all 16-character inputs. Cryptol currently only supports proofs of [total](https://en.wikipedia.org/wiki/Partial_function#Function) [monomorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) properties with a finite domain. Here we must specify the length of the messages that we want to check this property for. This example checks the property for 16 character messages, but we could check this for any (reasonable) length.

# Language Features

So what makes Cryptol special compared to other languages?

Cryptol is a language designed with Cryptography specifically in mind
-- much of the syntax and language was designed to align with the way
that real cryptographers think about and design systems. This allows
the Cryptol user to create formal algorithm specifications that
closely imitate the style used to describe these algorithms
mathematically.

Furthermore, Cryptol provides direct access to and easily integrates
with powerful tools such as SMT solvers and the Software Analysis
Workbench (SAW). These tools allow the user to *prove* facts and
demonstrate properties about their code which can provide assurance
guarantees that go far beyond simple unit testing.

A detailed introduction to the Cryptol interpreter is provided next in
the [Interpreter lab](../Interpreter/Interpreter.md)

# References

* [Programming Cryptol](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf) -- A good overview and reference for the Cryptol language. Contains many examples and references for programming language features.

* [Cryptol Version 2 Syntax](https://github.com/GaloisInc/cryptol/blob/master/docs/Syntax.pdf) -- A comprehensive guide to Cryptol Syntax

* [Cryptol Semantics](https://github.com/GaloisInc/cryptol/blob/master/docs/Semantics.pdf) -- A guide to Cryptol language semantics and overview of the underlying representations of Cryptol's type system

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [^ Course README](../../README.md) ||
| [< Installation](../../INSTALL.md) | **Overview** | [Interpreter >](../Interpreter/Interpreter.md) |
