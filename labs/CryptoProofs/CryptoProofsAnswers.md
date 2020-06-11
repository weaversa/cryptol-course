# Exploring Cryptography with Cryptol's Proof Tools

Cryptol and SAW allow users to rapidly and transparently deploy powerful theorem proving tools to explore their code.

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. By the end of this lab, the student will be able to describe and demonstrate five powerful classes of proofs that can be applied to a wide variety of cryptographic algorithms.

First, since we are creating a module, the first line needs to be the
module definition.

```
module labs::CryptoProofs::CryptoProofsAnswers where
```

## 1. DES

In this lab, we'll analyze the DES (Data Encryption Standard) algorithm. Let's take a moment to familiarize ourselves with how it works.

First, we import it.

```
import specs::DES
```

Now, from the command line, load this module.

```bash
Cryptol> :m labs::CryptoProofs::CryptoProofs
Loading module labs::CryptoProofs::CryptoProofs
```

 Let's encrypt something with DES.

```bash
labs::CryptoProofs::CryptoProofs> DES.encrypt 0x752878397493cb70 0x1122334455667788
0xb5219ee81aa7499d
```

 Now decrypt:

 ```bash
 labs::CryptoProofs::CryptoProofs> DES.decrypt 0x752878397493cb70 0xb5219ee81aa7499d
 0x1122334455667788
 ```

Now that we have DES working, let's analyze it!

## 2. Killer Apps

For the rest of the lab, we'll be looking at some of the types of questions you can ask (and sometimes even answer!) using Cryptol's powerful automated theorem proving capabilities. Here are important questions that one might ask about a cryptographic algorithm along with a generic "one-liner" Cryptol invocation. (Don't worry if you don't understand these yet.)

* Function reversal: `:sat \x -> f x == y`
* Proof of inversion: `:prove \x -> f (g x) == x`
* Collision detection: `:sat \(x,y) -> f x == f y /\ x != y`
* Absence of collisions: `:prove \(x,y) -> x != y ==> f x != f y`
* Equivalence checking: `:prove \x -> f x == g x`

Each subsection below will explore one of these questions in-depth.

### 2.1 Function Reversal

It may be interesting to explore whether a particular cryptographic function can be reversed. Some examples of usage:

* Attempt to reverse a hash function. This is called a preimage attack. (Of course, strong cryptographic hash functions are designed to resist this type of analysis.)
* Carry out decryption when all you have is the encryption function.
* In general, find an input given an output!

We'll start with an example where we reverse the following simple function:

```
square : Integer -> Integer
square x = x * x
```

Now we can reverse it from the REPL. Let's use the solver to find a square root using only a squaring function!

```bash
labs::CryptoProofs::CryptoProofs> :sat \x -> square x == 1764
(\x -> square x == 1764) 42 = True
(Total Elapsed Time: 0.021s, using "Z3")
```

Let's take a closer look at this query, which makes use of a *lambda* (anonymous/unnamed/on-the-fly) function. Here's the breakdown:

|||||
| --- | --- | --- | --- |
| `:sat`            | `\x`                   | `->`        | `square x == 1764` |
| "Hey SAT solver!" | "Find me an input *x*" | "such that" | "x squared equals 1764" |
| Asks the SAT solver to find an input(s) | Lamba function input | More lambda syntax | Function body |

#### Exercise 2.1.1: Reverse DES.encrypt

Given the following key and ciphertext, find the plaintext using only the solver and the `DES.encrypt` function.

```
key = 0x752878397493cb70
ct = 0xf2930290ea4db580
```

Note: For whatever reason, the default Z3 solver has trouble with this one. Try one of the other solvers, such as yices:

```bash
labs::CryptoProofs::CryptoProofsAnswers> :s prover=yices
```

Or use all the installed solvers in a first-to-the-post race. (Careful! May be too much for your system.)

```bash
labs::CryptoProofs::CryptoProofsAnswers> :s prover=any
```

Solution:

```bash
labs::CryptoProofs::CryptoProofsAnswers> :s prover=any
labs::CryptoProofs::CryptoProofsAnswers> :sat \pt -> DES.encrypt key pt == ct
(\pt -> DES.encrypt key pt == ct) 0x70617373776f7264 = True
(Total Elapsed Time: 0.719s, using "Yices")
```

### 2.2 Proof of Inversion