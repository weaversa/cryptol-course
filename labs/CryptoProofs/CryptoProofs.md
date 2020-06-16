# Exploring Cryptography with Cryptol's Proof Tools

Cryptol and SAW allow users to rapidly and transparently deploy powerful theorem proving tools to explore their code.

By the end of this lab, the student will be able to describe and demonstrate five powerful classes of proofs that can be applied to a wide variety of cryptographic algorithms.

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter.

First, since we are creating a module, the first line needs to be the
module definition.

```
module labs::CryptoProofs::CryptoProofs where
```

# 1. DES

To start, we'll analyze the DES (Data Encryption Standard) algorithm. Let's take a moment to familiarize ourselves with how it works.

First, we import it.

```
import specs::DES
```

Now, from the command line, load this module.

```bash
Cryptol> :m labs::CryptoProofs::CryptoProofs
Loading module labs::CryptoProofs::CryptoProofs
```

First, we'll take a look at the type of the DES encryption function.

```bash
labs::CryptoProofs::CryptoProofs> :t DES.encrypt
DES.encrypt : [64] -> [64] -> [64]
```

DES takes two 64-bit values and returns a 64-bit value. (The key comes first and then the plaintext.) Let's encrypt something with DES.

```bash
labs::CryptoProofs::CryptoProofs> DES.encrypt 0x752979387592cb70 0x1122334455667788
0xb5219ee81aa7499d
```

 Now decrypt:

 ```bash
 labs::CryptoProofs::CryptoProofs> DES.decrypt 0x752979387592cb70 0xb5219ee81aa7499d
 0x1122334455667788
 ```

Now that we have DES working, let's analyze it!

# 2. Four Killer Apps

For the rest of the lab, we'll be looking at some of the types of questions you can ask (and often answer!) using Cryptol's powerful automated theorem proving capabilities. These are important questions that one might ask about a cryptographic algorithm along with a generic "one-liner" Cryptol invocation. (Don't worry if you don't understand these yet.)

| Proof | Invocation |
|-|-|
| Function reversal | `:sat \x -> f x == y` |
| Proof of inversion | `:prove \x -> g (f x) == x` | 
| Collision detection | `:sat \(x,y) -> f x == f y /\ x != y` |
| Equivalence checking | `:prove \x -> f x == g x` |

Each subsection below will explore one of these questions in-depth.

## 2.1 Function Reversal

It may be interesting to explore whether a particular cryptographic function can be reversed. Some examples of usage:

* Attempt to reverse a hash function. This is called a preimage attack. (Of course, strong cryptographic hash functions are designed to resist this type of analysis.)
* Carry out decryption when all you have is the encryption function.
* In general, find an input given an output!

We'll start with an example where we reverse the following simple function:

```
// square - multiplies an integer by itself
```

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

|Function Reversal||||
|-|-|-|-|
| `:sat`            | `\x`                   | `->`        | `square x == 1764` |
| "Hey SAT solver!" | "Find me an input `x`" | "such that" | "`x` squared equals `1764`" |
|||||

### Exercise 2.1.1 Reverse DES.encrypt

Given the following key and ciphertext, find the plaintext using only the solver and the `DES.encrypt` function.

```
known_key = 0x752979387592cb70
known_ct = 0xf2930290ea4db580
```

Note: For whatever reason, the default Z3 solver has trouble with this one. Try one of the other solvers, such as yices:

```bash
labs::CryptoProofs::CryptoProofs> :s prover=yices
```

Or use all the installed solvers in a first-to-the-post race.
*Caution! May exhaust system resources.*

```bash
labs::CryptoProofs::CryptoProofs> :s prover=any
```

### Exercise 2.1.2 Breaking DES

Given the following matched plaintext and ciphertext, ask the solver to find the key. Will this work? Why or why not? (*Hint: see plaintext.*) Note that you can stop the solver at any time by hitting `ctrl-c`.

```
matched_pt = join "tootough"
matched_ct = 0x95d07f8a72707733
```

To make this solvable, try it again with the first six bytes of key provided: `0x1234567890ab`.

## 2.2 Proof of Inversion

For symmetric ciphers, it is necessary that the decrypt function *inverts* the encrypt function. (That is, it restores the ciphertext to the original plaintext.) It is easy to express this property in Cryptol.

Consider the following functions `f` and `g`:

```
f: Integer -> Integer
f x = 3 * x + 2
g: Integer -> Integer
g x = (x - 2) / 3
```

We want to prove that function `g` inverts function `f`; that is, applying `g` to the result of `f x` gets `x` back. Here's the invocation:

```bash
labs::CryptoProofs::CryptoProofs> :prove \x -> g (f x) == x
Q.E.D.
(Total Elapsed Time: 0.023s, using "Z3")
```

Here's the breadown of this proof:

|Proof of Inversion||||
|-|-|-|-|
| `:prove`            | `\x`                   | `->`        | `g (f x) == x` |
| "Prove to me" | "that for all `x`" | "it is true that" | "`g` inverts `f`" |
|

### Exercise 2.2.1 The other direction

Our example proof showed that `g` inverts `f` for all inputs. Does this work the other way around? Try it! If the proof fails, it will provide a *counterexample*. Use the counterexample to understand what went wrong.

### Exercise 2.2.2 DES inversion

Use Cryptol to prove that `DES.encrypt` and `DES.decrypt` are inverses for all possible inputs. Show both directions.

*Hint*: Note that this function has more than one input. To make it work, you can either use a tuple `(,)` (tuple-style) or a chain of of `\->` (Curried-style) constructs for your lambda function input.

*Hint*: For fastest results, use the `abc` prover.

## 2.3 Collision Detection

In cryptography, a *collision* occurs when two different inputs produce the same output. For some cryptographic functions, such as pseudo-random number generators (PRNGs), it may be desirable to demonstrate an absence of collisions. In other functions, such as cryptographic hash functions, collisions are inevitable, but should be difficult to discover. It is easy in Cryptol to ask the solver to search for collisions. (Though finding a solution may not be possible.)

### Exercise 2.3.1 DES Key Collisions

Use the solver to find two different keys and a plaintext such that both keys encrypt that plaintext to the same ciphertext.

```bash
labs::CryptoProofs::CryptoProofs> :s prover=yices
labs::CryptoProofs::CryptoProofs> :sat \(k1,k2,pt) -> k1 != k2 /\ DES.encrypt k1 pt == DES.encrypt k2 pt
(\(k1, k2, pt) -> k1 != k2 /\ DES.encrypt k1 pt == DES.encrypt k2
                                                               pt)
  (0x0000000000000000, 0x0100000000000000, 0x0000000000000000) = True
(Total Elapsed Time: 1.662s, using Yices)
```

## 2.4 Equivalence Checking

One of the most powerful uses of Cryptol's theorem proving technology is the ability to show equivalence of two different implementations for all possible inputs. One way to use this is to create two functions:

1. A *specification* that is written for clarity and correctness.
2. An *implementation* that is optimized and intended for usage.

With a strong specification in hand, a user can aggressively optimize, confident that, given that the equivalence proof passes, their implementation is correct.

Using the Software Analysis Workbench, a powerful framework built on Cryptol, it is even possible to carry out equivalence checks (and all of the proofs from this lab as well) on implementations in languages such as C and Java. (For an introduction to SAW, see other labs in the course.)

### Exercise 2.4.1 DES Equivalent Keys

It's inevitable that there are collisions over the set of all key/plaintext pairs in DES, but it may be surprising that they're easy to find with Cryptol's solver. We now know that the two keys you just found encrypt one particular plaintext to the same ciphertext; more concerning would be if they perform the same transformation on *all* plaintexts. Such keys are called *equivalent* keys.

Attempt to prove that the two keys you just found are equivalent keys. That is, prove that these two keyed DES functions are equivalent for all plaintext inputs. *Hint: Use abc*

### Exercise 2.4.2 DES Parity Bits

Having equivalent keys is often considered a weakness in an a cipher. However, in the case of DES, it turns out that this is a result of a design choice. The lowest bit of each byte of a DES key is actually a *parity* bit that is completely ignored by the cihper itself. The value of the parity bit is such that each byte has an odd number of bits set.

Write a function `DESFixParity : [64] -> [64]` that takes any 64-bit vector and returns the equivalent DES key properly computed parity bits.

### Exercise 2.4.3 Proving DES Key Equivalence

Use the function `DESFixParity` that you wrote above to show that DES completely ignores parity bits. That is, prove that the DES encryption that allows all 64-bit keys is equivalent to the DES encryption function that first corrects the parity bits on those keys.

Given that this proof passes, what is the actual maximum key strength of DES in terms of bits?

# The end

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course Github page:
https://github.com/weaversa/cryptol-course/issues