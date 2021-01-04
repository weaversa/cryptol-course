# Introduction

Cryptol and SAW allow users to rapidly and transparently deploy
powerful theorem proving tools to explore their code.

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in this file.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * sequence and `Integer` types,
  * the `:prove` and `:sat` commands,
  * manipulating sequences using `#`, `take`, `split`, and `join`,
  * writing functions and properties,
  * lambda functions,
  * sequence comprehensions, and
  * logical, comparison, and arithmetic operators.

## Skills You'll Learn

By the end of this lab you will be able to describe and demonstrate
five powerful classes of proofs that can be applied to a wide variety
of cryptographic algorithms.

You'll also gain experience with
  * lambda functions,
  * `:prove` and `:sat` commands, and
  * different provers.
  
## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::CryptoProofs::CryptoProofs
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofs
```

The proofs in this lab require an array of different theorem provers
supported by Cryptol. In order to solve them, we recommend using the
Cryptol Docker container described in the README.md for this course.

First, since we are creating a module, the first line needs to be the
module definition.

```cryptol
module labs::CryptoProofs::CryptoProofs where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Exploring Cryptography with Cryptol's Proof Tools

## 1. DES

To start, we'll analyze the DES (Data Encryption Standard)
algorithm. Let's take a moment to familiarize ourselves with how it
works.

First, we import it.

```cryptol
import specs::Primitive::Symmetric::Cipher::Block::DES
```

When you loaded the `labs::CryptoProofs::CryptoProofs` module, these
lines should have been printed:

```Xcryptol-session
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofs
```

In reverse order: the third line says that this module has been
loaded.  Since it imported the DES module, Cryptol helpfully tells you
that DES has been loaded.  Since DES imported the Cipher module,
Cryptol tells you that too.

Next, we'll take a look at the type of the DES encryption function.

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> :t DES.encrypt
DES.encrypt : [64] -> [64] -> [64]
```

DES takes two 64-bit values and returns a 64-bit value. (The key comes
first and then the plaintext.) Let's encrypt something with DES.

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> DES.encrypt 0x752979387592cb70 0x1122334455667788
0xb5219ee81aa7499d
```

Now decrypt:

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> DES.decrypt 0x752979387592cb70 0xb5219ee81aa7499d
0x1122334455667788
```

Now that we have DES working, let's analyze it!

## 2. Five Killer Apps

For the rest of the lab, we'll be looking at some of the types of
questions you can ask (and often answer!) using Cryptol's powerful
automated theorem proving capabilities. These are important questions
that one might ask about a cryptographic algorithm along with a
generic "one-liner" Cryptol invocation. (Don't worry if you don't
understand these yet.)

| Proof | Invocation |
|-|-|
| Function reversal | `:sat \x -> f x == y` |
| Proof of inversion | `:prove \x -> g (f x) == x` |
| Proof of injectivity | `:prove \x y -> x != y ==> f x != f y` |
| Collision detection | `:sat \x y -> f x == f y /\ x != y` |
| Equivalence checking | `:prove \x -> f x == g x` |

Each subsection below will explore one of these questions in-depth.

### 2.1 Function Reversal

It may be interesting to explore whether a particular cryptographic
function can be reversed. Some examples of usage:

* Attempt to reverse a hash function. This is called a preimage
  attack. (Of course, strong cryptographic hash functions are designed
  to resist this type of analysis.)
* Carry out decryption when all you have is the encryption function.
* In general, find an input given an output!

We'll start with an example where we reverse the following simple
function:

```cryptol
/** square - multiplies an Integer by itself */

square : Integer -> Integer
square x = x * x
```


Now we can reverse it from the REPL. Let's use the solver to find a
square root using only a squaring function!

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> :sat \x -> square x == 1764
Satisfiable
(\x -> square x == 1764) 42 = True
(Total Elapsed Time: 0.021s, using "Z3")
```

Let's take a closer look at this query, which makes use of a *lambda*
(anonymous/unnamed/on-the-fly) function. Here's the breakdown:

|Function Reversal||||
|-|-|-|-|
| `:sat`            | `\x`                   | `->`        | `square x == 1764` |
| "Hey SAT solver!" | "Does there exist an `x`" | "such that" | "`x` squared equals `1764`?" |
|||||

For more information on lambda functions in Cryptol, see Section
1.13.3 of [Programming
Cryptol](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf).

**EXERCISE**: 2.1.1 Reverse DES.encrypt

Given the following key and ciphertext, find the plaintext using only
the solver and the `DES.encrypt` function. To do this, head over to
the Cryptol interpreter, load up the module, and use the `:sat`
command similar to the example above.

```cryptol
known_key = 0x752979387592cb70
known_ct = 0xf2930290ea4db580
```

Note: For whatever reason, the default Z3 solver has trouble with this
one. Try one of the other solvers, such as YICES:

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> :s prover=yices
```

Or use all the installed solvers in a first-to-the-post race.
*Caution! May exhaust system resources.*

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> :s prover=any
```


**EXERCISE**: 2.1.2 Breaking DES

Given the following matched plaintext and ciphertext, ask the solver
to find the key. Will this work? Why or why not? (*Hint*: see
plaintext.) Note that you can stop the solver at any time by hitting
`ctrl-c`.

```cryptol
matched_pt = join "tootough"
matched_ct = 0x95d07f8a72707733
```

To make this solvable, try it again with the first six bytes of key
provided: `0x1234567890ab`.


### 2.2 Proof of Inversion

For symmetric ciphers, it is necessary that the decrypt function
*inverts* the encrypt function. (That is, it restores the ciphertext
to the original plaintext.) It is easy to express this property in
Cryptol.

Consider the following functions `f` and `g`:

```cryptol
f: Integer -> Integer
f x = 3 * x + 2
g: Integer -> Integer
g x = (x - 2) / 3
```

We want to prove that function `g` inverts function `f`; that is,
applying `g` to the result of `f x` gets `x` back. Here's the
invocation:

```Xcryptol-session
labs::CryptoProofs::CryptoProofs> :prove \x -> g (f x) == x
Q.E.D.
(Total Elapsed Time: 0.023s, using "Z3")
```

Here's the breakdown of this proof:

|Proof of Inversion||||
|-|-|-|-|
| `:prove`            | `\x`                   | `->`        | `g (f x) == x` |
| "Prove to me" | "that for all `x`" | "it is true that" | "`g` inverts `f`" |
|||||

**EXERCISE**: 2.2.1 The other direction

Our example proof showed that `g` inverts `f` for all inputs. Does
this work the other way around? Try it! If the proof fails, it will
provide a *counterexample*. Use the counterexample to understand what
went wrong.


**EXERCISE**: 2.2.2 DES inversion

Use Cryptol to prove that `DES.encrypt` and `DES.decrypt` are inverses
for all possible inputs. Show both directions.

*Hint*: Lambda functions can take more than one input, just like
normal functions! For example: `\x y z -> x+y+z`

*Hint*: For fastest results, use the `abc` prover.


### 2.3 Proof of Injectivity

A function for which every input generates a distinct output is
referred to in mathematics as *injective* or *one-to-one*. Cryptol can
be used to prove that a function is injective.

**EXERCISE**: 2.3.1 DES Injectivity

Show that, for any given key, `DES.encrypt` is injective
(collision-free) with respect to plaintext.

Technically, `DES.encrypt` (for any given key) is also *surjective*
(*onto*) due to the fact that its domain and range are the same (The
set of all possible 64-bit vectors.) A function that is both injective
and surjective is called *bijective*.

*Hint*: Use the Boolector prover. (Even then, this proof may take a
few minutes!)

*Hint*: Consider using the implication operator `==>`


### 2.4 Collision Detection

In cryptography, a *collision* occurs when two different inputs
produce the same output. (That is, the function is *not* injective.)
For some cryptographic functions, such as pseudo-random number
generators (PRNGs), it may be desirable to demonstrate an absence of
collisions. In other functions, such as cryptographic hash functions,
collisions are inevitable, but should be difficult to discover. It is
easy in Cryptol to ask the solver to search for collisions. (Though
finding a solution may not be possible.)

**EXERCISE**: 2.4.1 DES Key Collisions

Use the solver to find two different keys and a single plaintext such
that both keys encrypt that plaintext to the same ciphertext.


### 2.5 Equivalence Checking

It's inevitable that there are collisions over the set of all
key/plaintext pairs in DES, but it may be surprising that they're easy
to find with Cryptol's solver. We now know that the two keys you just
found encrypt one particular plaintext to the same ciphertext; more
concerning would be if they perform the same transformation on *all*
plaintexts. Such keys are called *equivalent* keys.

One of the most powerful uses of Cryptol's theorem proving technology
is the ability to show equivalence of two different functions for all
possible inputs.

**EXERCISE**: 2.5.1 DES Equivalent Keys

Prove that the two keys you just found are equivalent keys. That is,
prove that these two keyed DES functions are equivalent for *all*
plaintext inputs.

*Hint*: Use the `abc` prover.


**EXERCISE**: 2.5.2 DES Parity Bits

Having equivalent keys is often considered a weakness in an a
cipher. However, in the case of DES, it turns out that this is a
result of a design choice. The lowest bit of each byte of a DES key is
actually a [parity bit](https://en.wikipedia.org/wiki/Parity_bit) that
is completely ignored by the cipher itself. For DES, the parity bit
ensures that the total number of 1-bits in each byte is odd.

Write a function `DESFixParity : [64] -> [64]` that takes any 64-bit
vector and returns the equivalent DES key with properly computed
parity bits. (This is the first and only time in this lab that you'll
need to edit this file directly.)

```cryptol
DESFixParity : [64] -> [64]
DESFixParity key = undefined // Replace "undefined" with your code
```

**EXERCISE**: 2.5.3 Proving DES Key Equivalence

Use the function `DESFixParity` that you wrote above to show that DES
completely ignores parity bits. That is, prove that the DES encryption
that allows all 64-bit keys is equivalent to the DES encryption
function that first corrects the parity bits on those keys.

Given that this proof passes, what is the actual maximum key strength
of DES in terms of bits?

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!
||||
|-:|:-:|-|
|| [^ Course README](../../README.md) ||
| [< Salsa20](../Salsa20/Salsa20.md) | **Cryptographic Properties** | [Key Wrapping >](../KeyWrapping/KeyWrapping.md) |
|| [! Cryptographic Properties (Answers)](./CryptoProofsAnswers.md) ||
||||
|| [+ Salsa20 Properties](../Salsa20/Salsa20Props.md) ||
|| [+ Transposition Ciphers](../Transposition/Contents.md) ||
|| [+ Project Euler](../ProjectEuler/ProjectEuler.md) ||
