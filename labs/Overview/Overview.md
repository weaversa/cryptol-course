# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```shell
cryptol> :module labs::Overview::Overview
```

Cryptol is a domain specific language and tool suite created by Galois, Inc., 
with support from NSA cryptographers. The language has lots of cool programming
language features that make it well suited for applications in high assurance 
systems and cryptographic development, including:

 * strong, static typing
 * type inference
 * parametric size-polymorphism
 * higher-order functions

Cryptol is used to create gold-standard algorithm specifications and provides 
access to tools that facilitate their exploration and evalution.

This lab will provide a quick overview of Cryptol, some motivating applications 
where the language and technology have been deployed, and to the language 
features which make Cryptol an excellent choice for these applications.

More information about Cryptol is available at [http://www.cryptol.net](https://cryptol.net).

# First Steps: Hello, Cryptol!

A grand tradition when learning a new programming language is to test the waters with a Hello World program. In this file we've defined a function `sayHello` which is specified as follows:

```
module labs::Overview::Overview where

sayHello : {a} (fin a) => [a][8] -> [7+a][8]
sayHello name = greeting
  where
    greeting = "Hello, " # name
```

If you have Cryptol installed, you should be able to do the following from the terminal:

```sh
$ cryptol
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.1 (ce0365f)

Loading module Cryptol
Cryptol> :module labs::Overview::Overview
Loading module labs::Overview::Overview
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

These security property tests are performed as part of a continuous integration pipline using the [Travis Continuous Integration Service](https://travis-ci.com/). Whenever changes are made to the C implementations (no matter how small), Cryptol and SAW evaluations are automatically run to ensure that no security properties of the system have been disrupted by the proposed updates.

A thorough description of the research, design decisions, and application of Cryptol to evaluating cryptographic implementations in Amazon's s2n system can be found in the paper [Contiuous Formal Verification of Amazon s2n](https://link.springer.com/chapter/10.1007/978-3-319-96142-2_26). This paper was selected by NSA's [Science of Security](https://www.nsa.gov/what-we-do/research/science-of-security/) group for honorable mention in the [7th Annual Best Scientific Cybersecurity Paper Competition](https://cps-vo.org/group/sos/papercompetition/pastcompetitions).

You can review the code for yourself on [Amazon's s2n Github Repository](https://github.com/awslabs/s2n). The code relevant to the specification and evaluation of the HMAC routines can be found in the `tests/saw/` directory.

Further exposition on the development of these integration tests can be found in a three part series on the [Galois Inc. Blog](https://galois.com/blog/):
 * [Part 1](https://galois.com/blog/2016/09/verifying-s2n-hmac-with-saw/) - **Verifying s2n HMAC with SAW**
 * [Part 2](https://galois.com/blog/2016/09/specifying-hmac-in-cryptol/) - **Specifying HMAC in Cryptol**
 * [Part 3](https://galois.com/blog/2016/09/proving-program-equivalence-with-saw/) - **Proving Program Equivalence with SAW**

## Verifying Cryptographic Implementations: The `xxhash` Algorithm

The tools that Cryptol provides allows users to bring together cryptographic implementations from other languages like *Java* or *C* and prove that they are equivalent to "gold standard" specifications one can create in Cryptol. This allows users to iteratively optimize code in performance-centric or system languages while maintaining a single trusted specification in Cryptol.

Take a look through the [saw-demos repository](https://github.com/GaloisInc/saw-demos) by Galois which highlights several of these applications. We will do a brief survey of the `xxhash` example which you can find in the `demos/xxhash` directory of this repository. This is an example of using Cryptol algorithm specifications (along with SAW). *(You can clone this repository and perform the exercises yourself if you like.)*

This directory contains the following files:

```sh
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
Finally the files [```xxhash32-ref.saw```](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash32-ref.saw) and [```xxhash64-ref.saw```](https://github.com/GaloisInc/saw-demos/blob/master/demos/xxhash/xxhash64-ref.saw) contain SAW scripts which drive the verification that this `C` code is equivalent to the specification found in this Cryptol specification of `xxhash`.

If you have installed the `saw-demos` repo, running `make` at the command line will initiate the verification for both the 32- and 64-bit implementations, producing the following output:

```sh
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

These scripts will check that the `C` implementations match the Cryptol specification for *every possible* input for the hash lengths specified. This is important to highlight because this is far beyond the capability of unit testing to detect errors. For instance, for inputs of length `128` bits, there are `2^160` input/seed combinations to check. Unit tests -- even random unit tests -- may only typically cover a few hundred or thousand cases. Cryptol and SAW are able to provide confidence on a space many orders of magnitude larger.

## Verifying Properties about Algorithms

Cryptol provides an easy interface for using powerful tools such as SMT solvers for verifying properties about algorithms we care about. Throughout this course, we will introduce examples and explain how to take advantage of these tools in your own designs and evaluations. Here is an example packaged with the Cryptol source that demonstrates a simple but important property about an encryption algorithm which only uses the `XOR` operation:

```
encrypt : {a}(fin a) => [8] -> [a][8] -> [a][8]
encrypt key plaintext = [ pt ^ key | pt <- plaintext ]

decrypt : {a}(fin a) => [8] -> [a][8] -> [a][8]
decrypt key ciphertext = [ ct ^ key | ct <- ciphertext ]

property roundtrip key plaintext = decrypt key (encrypt key plaintext) == plaintext
```

This file defines an `encrypt` operation, a `decrypt` operation, and a property called `roundtrip` which checks for all keys `key` and all input plaintexts `plaintext` that `decrypt key (encrypt key plaintext) == plaintext` (*i.e.* that these operations are inverse to one another).

We can see the effect of encrypting the particular input `attack at dawn` with the key `0xff`:

```sh
labs::overview::overview> encrypt 0xff "attack at dawn"
[0x9e, 0x8b, 0x8b, 0x9e, 0x9c, 0x94, 0xdf, 0x9e, 0x8b, 0xdf, 0x9b,
 0x9e, 0x88, 0x91]
```

Cryptol interprets the string `"attack at dawn"` as a sequence of bytes suitable for the encrypt operations. (We will introduce Cryptol types later in this lab and discuss them in detail throughout this course.)

Furthermore, we can prove this property holds in the interpreter using the `:prove` command and the currently configured SMT solver (Z3 by default):

```sh
labs::overview::overview> :prove roundtrip : [8] -> [16][8] -> Bit
Q.E.D.
(Total Elapsed Time: 0.010s, using Z3)
```

Cryptol reports `Q.E.D.`, indicating that our property is indeed true for all keys and all 16-character inputs. Cryptol currently only supports proofs of [total](https://en.wikipedia.org/wiki/Partial_function#Function) [monomorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) properties with a finite domain. Here we must specify the length of the messages that we want to check this property for. This example checks the property for 16 character messages, but we could check this for at any (reasonable) length.

# Language Features

So what makes Cryptol special compared to other languages?

Cryptol is a language designed with Cryptography specifically in mind -- much of the syntax and language was designed to align with the way that real cryptographers think about and design systems. This allows the Cryptol user to create formal algorithm specifications that closely imitate the style used to describe these algorithms mathematically.

Furthermore, Cryptol provides direct access to and easily integrates with powerful tools such as SMT solvers and the Software Analysis Workbench (SAW). These tools allow the user to *prove* facts and demonstrate properties about their code which can provide assurance guarantees that go far beyond simple unit testing.

We will introduce some of these features below and discuss how they support building Cryptographic specifications and evaluations. If you have access to the Cryptol interpreter, you can follow along with some of the examples; detailed introduction to the Cryptol interpreter will be provided in future lessons.

## Basic Data Types

Cryptol was designed to provide easy access to the sorts of data and operations that appear in Cryptographic algorithms and specifications. There are five basic data types provided by Cryptol: bits, sequences, integers, tuples, and records. Cryptol also supports the ability to create user-defined types built up from the basic types. In this section, we present some basic examples demonstrating these types; note that commands using `:type` are a request to Cryptol to report the type of argument.

 * **Bits** - The simplest data type, bits can take on two values: `True` and `False`. Single-bit operations like `and` (`/\`), `or` (`\/`), and `not` (`~`) are available.

```haskell
Cryptol> :type True
True : Bit
Cryptol> True /\ False
False
Cryptol> True \/ False
True
Cryptol> ~True
False
```

 * **Sequences** - Finite lists of objects all of the same data type. Common cryptographic algorithms make use of *words* or *registers*, which are one-dimensional sequences of Bits. Cryptol seemlessly handles operations on words of arbitrary sizes and also allows for multi-dimensional sequences (*i.e.* sequences of words, or sequences of sequences of words).

```haskell
Cryptol> let s1 = [True, True, False, True]
Cryptol> :type s1
s1 : [4]
Cryptol> s1
0xd
Cryptol> let s2 = [0x1f, 0x11, 0x03, 0xd5]
Cryptol> :type s2
s2 : [4][8]
Cryptol> let s3 = [[0x1, 0x2], [0x3, 0x4], [0x5, 0x6]]
Cryptol> :type s3
s3 : [3][2][4]
```
 
 * **Integers** - An arbitrary precision integer type.

```haskell
Cryptol> 1+1 : Integer
2
Cryptol> 42*314 : Integer
13188
Cryptol> 123456789234567890+234567890123456789 : Integer
358024679358024679
```

 * **Tuples** - Tuples support heterogenous collections. Members are accessed with the `dot operator` (`.`) and are zero-indexed.
 
```haskell
Cryptol> let tup = (1 : Integer, 0x02, [0x31, 0x32])
Cryptol> :type tup
tup : (Integer, [8], [2][8])
Cryptol> tup.0
1
Cryptol> tup.1
0x02
Cryptol> tup.2
[0x31, 0x32]
```

 * **Records** - Records allow for more complex data structures to be formed, where fields may be accessed by their names.
 
```haskell
Cryptol> let point = {x = 10:[32], y = 25:[32]}
Cryptol> point.x
0x0000000a
Cryptol> point.y
0x00000019
```

## Operators

Cryptol provides a collection of built-in operators to build expressions and perform computations. Some familiar operators which appear in cryptographic applications include:

* `~`, `/\`, `\/` -- single bit logical operations
* `&&`, `||`, `^` -- logical operations for sequences
* `==`, `!=` -- structural comparison
* `==`, `>=`, `>`, `<=`, `<` -- nonnegative word comparisons
* `+`, `-`, `*`, `/`, `%`, `**` -- wordwise modular arithmetic
* `>>`, `<<`, `>>>`, `<<<` -- shifts and rotates
* `#` -- concatenation

There are many more. A list of the currently defined symbols and operators are available by typing the `:browse` command in the interpreter.

An interesting feature of Cryptol's type system is that operators are typed. You can check this with the `:type` command in the interpreter just as you can for data:

```sh
labs::overview::overview> :type (&&)
(&&) : {a} (Logic a) => a -> a -> a
```

In a nutshell, this indicates that the bitwise `and` operator (`&&`) operates on two elements of type `a` that are in the "`Logic`" typeclass and returns another element of the same type. Sometimes this information is useful for debugging a design.

## Primitives

Cryptol offers a robust set of primitives to construct more complex functions and specifications. The built-in primitives cover a wide variety of purposes. A sampling of these built-ins include:

 * **Sequence manipulation** -- `take`, `drop`
 
```sh
labs::overview::overview> take `{1} [1, 2, 3]
[1]
labs::overview::overview> drop `{1} [1, 2, 3]
[2, 3]
```
 
 * **Structural manipulation** -- `split`, `groupBy`

```sh
labs::overview::overview> split `{3} [1, 2, 3, 4, 5, 6]
[[1, 2], [3, 4], [5, 6]]
labs::overview::overview> groupBy `{3} [1, 2, 3, 4, 5, 6]
[[1, 2, 3], [4, 5, 6]]
```

 * **Arithmetic** -- `sum`, `min`, `max`

```sh
labs::overview::overview> sum [1, 2, 3, 4, 5]
15
labs::overview::overview> min 5 10
5
labs::overview::overview> max 5 10
10
```

In the Cryptol interpreter, you can inspect the symbols, functions, and primitives that are defined within the current context using the `:browse` (or simply `:b`) command. Doing so at the beginning of a fresh interpreter session will show you the list of built-ins and other information about the environment:

```sh
labs::overview::overview> :b
    all : {n, a} (fin n) => (a -> Bit) -> [n]a -> Bit
    and : {n} (fin n) => [n] -> Bit
    any : {n, a} (fin n) => (a -> Bit) -> [n]a -> Bit
    ...
    zext : {m, n} (fin m, m >= n) => [n] -> [m]
    zip : {n, a, b} [n]a -> [n]b -> [n](a, b)
    zipWith : {n, a, b, c} (a -> b -> c) -> [n]a -> [n]b -> [n]c
```


## Functions

Like many languages, Cryptol enables users to define functions, thereby allowing subcomponents to be reused and providing greater clarity. Functions are typically specified in two parts: the *type definition* and the *function definition*.

Suppose that a project file `rotword.cry` contains the following code:

```
RotWord : [4][8] -> [4][8]
RotWord [a0, a1, a2, a3] = [a1, a2, a3, a0]
```

This provides a type for `RotWord` (`[4][8] -> [4][8]`) and a definition (`RotWord [a0, a1, a2, a3] = [a1, a2, a3, a0]`). Within the interpreter we can perform computations and check the type of this function as follows:

```sh
labs::overview::overview> RotWord [0x01, 0x02, 0x03, 0x04]
[0x02, 0x03, 0x04, 0x01]
labs::overview::overview> :type RotWord 
RotWord : [4][8] -> [4][8]
```

To enjoy greater overall confidence in a system under development, we may want to check properties about the components we build. Suppose we wanted to check that `RotWord` was the identity on sequences where all the elements were the same. We could add the following line to `rotword.cry`:

```
property check_identity x = RotWord [x, x, x, x] == [x, x, x, x]
```

Then from the interpreter we could check that this property was true:

```sh
labs::overview::overview> :prove
:prove check_identity
	Q.E.D.
(Total Elapsed Time: 0.005s, using Z3)
```

Building up more complex properties and relationships between the components of a system allow us to assert with very high confidence that the system is correct.

* **lambda expressions** / **anonymous functions** -- Cryptol lets users define functions without having to specify a name.  This can be useful in some circumstances, such as to formulate a function which will be the return value of another function. Lambda expressions have types and can be computed with just like regular functions. They are formed as follows:

```sh
labs::overview::overview> \(x:[32]) -> x*x
<function>
labs::overview::overview> :type \(x:[32]) -> x*x
(\(x : [32]) -> x * x) : [32] -> [32]
Main> (\(x:[32]) -> x*x) 3
0x00000009
```

## Enumerations

Cryptol supports compact notation for [Arithmetic Sequences](https://en.m.wikipedia.org/wiki/Arithmetic_progression) (which increase or decrease in regular intervals):

```sh
labs::overview::overview> [1, 2 .. 10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
labs::overview::overview> [1, 4 .. 12]
[1, 4, 7, 10]
labs::overview::overview> [10, 9 .. 0]
[10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
```

Cryptol is kind enough to inform us that it is making an assumption about the type in the sequence. Here is the full output:

```sh
labs::overview::overview> [1, 2 .. 10]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromThenTo'
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

We can nudge Cryptol with the element types to control the types of the elements of the enumeration:

```sh
labs::overview::overview> [0 .. 15 : [32]]
[0x00000000, 0x00000001, 0x00000002, 0x00000003, 0x00000004,
 0x00000005, 0x00000006, 0x00000007, 0x00000008, 0x00000009,
 0x0000000a, 0x0000000b, 0x0000000c, 0x0000000d, 0x0000000e,
 0x0000000f]
```

Furthermore, Cryptol even supports *infinite* sequences and accessing their members. Here is an example of creating an infinite sequence of odd integers and accessing the 100th element of that sequence:

```sh
labs::overview::overview> [1, 3 ... ] 
[1, 3, 5, 7, 9, ...]
labs::overview::overview> [1, 3 ... ] @ 100
201
```

Note that two dots (`..`) are used for constructing finite enumerations and three dots (`...`) are used for constructing infinite sequences.

## Sequence Comprehensions

Sequence Comprehensions are a technique for computing the elements of a new sequence out of the elements of existing ones. Here is a simple comprehension that computes the squares of the numbers from 1 to 10:

```sh
labs::overview::overview> [ x^^2 | x <- [1 .. 10]]
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
```

Comprehensions also support notions of *cartesian*, *parallel*, and *self-referential* definitions.

* **Cartesian** -- A new sequence is formed from all possible combinations of pairs taken from the supplied sequences when they are separated by commas. The size of a Cartesian comprehension will (in general) be the product of the sizes of the supplied sequences:

```sh
labs::overview::overview> [ (x, y) | x <- [0 .. 2], y <- [0 .. 2] ]
[(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
labs::overview::overview> [ x * y | x <- [0 .. 2], y <- [0 .. 2] ]
[0, 0, 0, 0, 1, 2, 0, 2, 4]
```

* **Parallel** -- Parallel definitions consume elements from multiple sequences simultaneously (when separated by a vertical bar `|`) and terminate when one sequence is exhausted. Typically, the size of a Parallel comprehension will be the minimum of the sizes of the supplied sequences:

```sh
labs::overview::overview> [ x + y | x <- [1 .. 10] | y <- [1 .. 10] ]
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
labs::overview::overview> [ x + y | x <- [1 .. 3] | y <- [1 .. 10] ]
[2, 4, 6]
```

* **Self-Referential** -- Sequences can even refer to themselves in a comprehension. This is a very powerful technique which is frequently used in cryptographic applications. Here we construct a representation of the infinite sequence of Fibonacci numbers. Note that here we assign a name to the sequence so that we can use it self-referentially in the definition:

```sh
labs::overview::overview> let fibs = [0, 1] # [ x + y | x <- fibs | y <- tail fibs ]
labs::overview::overview> fibs
[0, 1, 1, 2, 3, ...]
```

## Control Structures

Building formal specifications in Cryptol requires imitating the sorts of control structures that are commonly seen in code.

* `if ... then ... else ...` -- Conditional expressions in Cryptol work similar to the ternary conditional operator (`... ? ... : ...`) in C. This structure evaluates the first field, then evaluates the second field if `True` or the third field if `False`:

```sh
labs::overview::overview> if True then 0x2 else 0x3
0x2
labs::overview::overview> if False then 0x2 else 0x3
0x3
```

Cryptol conditionals are subject to typing conditions, and the two branches must have the same type to be a valid conditional expression. The type of a conditional expression is the shared type of the two branches.

```sh
labs::overview::overview> :type if True then 0x2 else 0x3
(if True then 0x2 else 0x3) : [4]
```

If the two branches are typed differently, Cryptol flags this as an error:

```sh
labs::overview::overview> if True then 2:[16] else 3:[32]

[error] at <interactive>:1:27--1:33:
  Type mismatch:
    Expected type: 16
    Inferred type: 32
```

To those unfamiliar with strongly typed languages, this may feel restrictive, but this is an important characteristic: it allows for precise specifications to be built and powerful evaluation tools to be levied against Cryptol specifications.

* `for ... next` loops -- Sequence comprehensions can be used to represent `for loops` from other languages. For example, consider the following snippet of C code that sums the numbers from 1 to 100:

```C
s = 0;
for( i = 1; i <= 100; i++) {
  s += i;
}
```

The following Cryptol snippet computes the same sequence of partial sums as the `for loop` above, and the result is found in the final element of this sequence:

```
ss = [0] # [ s + i | s <- ss | i <- [1 .. 100] ]
```

```sh
labs::overview::overview> ss
[0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136,
 153, 171, 190, 210, 231, 253, 276, 300, 325, 351, 378, 406, 435,
 465, 496, 528, 561, 595, 630, 666, 703, 741, 780, 820, 861, 903,
 946, 990, 1035, 1081, 1128, 1176, 1225, 1275, 1326, 1378, 1431,
 1485, 1540, 1596, 1653, 1711, 1770, 1830, 1891, 1953, 2016, 2080,
 2145, 2211, 2278, 2346, 2415, 2485, 2556, 2628, 2701, 2775, 2850,
 2926, 3003, 3081, 3160, 3240, 3321, 3403, 3486, 3570, 3655, 3741,
 3828, 3916, 4005, 4095, 4186, 4278, 4371, 4465, 4560, 4656, 4753,
 4851, 4950, 5050]

Cryptol> last ss
5050
```

Cryptol also has support for common functional programming concepts
such as `map`, `fold`, and `scan`. For example, a left fold (`foldl`)
can be used to compute the sum of the first 100 integers, like so:

```sh
labs::overview::overview> foldl (+) 0 [1..100]
5050
```

`foldl` takes a function (`+` in this case), an initial value (`0`), and a
sequence (`[1..100]`) and applies the function incrementally to the sequence,
starting with the initial value.

# Documentation and References

* [Programming Cryptol](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf) -- A good overview and reference for the Cryptol language. Contains many examples and references for programming language features.

* [Cryptol Version 2 Syntax](https://github.com/GaloisInc/cryptol/blob/master/docs/Syntax.pdf) -- A comprehensive guide to Cryptol Syntax

* [Cryptol Semantics](https://github.com/GaloisInc/cryptol/blob/master/docs/Semantics.pdf) -- A guide to Cryptol language semantics and overview of the underlying representations of Cryptol's type system

* [Cryptol Style Guide](cryptol-style.md) -- A set of guidelines for naming and formatting to enhance consistency and comprehensibility within Cryptol specificiations
