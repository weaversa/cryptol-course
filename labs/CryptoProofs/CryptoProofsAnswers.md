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
module labs::CryptoProofs::CryptoProofsAnswers where
```

## 1. DES

To start, we'll analyze the DES (Data Encryption Standard) algorithm. Let's take a moment to familiarize ourselves with how it works.

First, we import it.

```
import specs::DES
```

Now, from the command line, load this module.

```bash
Cryptol> :m labs::CryptoProofs::CryptoProofsAnswers
Loading module labs::CryptoProofs::CryptoProofsAnswers
```

First, we'll take a look at the type of the DES encryption function.

```bash
labs::CryptoProofs::CryptoProofsAnswers> :t DES.encrypt
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

## 2. Four Killer Apps

For the rest of the lab, we'll be looking at some of the types of questions you can ask (and often answer!) using Cryptol's powerful automated theorem proving capabilities. These are important questions that one might ask about a cryptographic algorithm along with a generic "one-liner" Cryptol invocation. (Don't worry if you don't understand these yet.)

| Proof | Invocation |
|-|-|
| Function reversal | `:sat \x -> f x == y` |
| Proof of inversion | `:prove \x -> g (f x) == x` | 
| Collision detection | `:sat \(x,y) -> f x == f y /\ x != y` |
| Equivalence checking | `:prove \x -> f x == g x` |
|

Each subsection below will explore one of these questions in-depth.

### 2.1 Function Reversal

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

#### Exercise 2.1.1 Reverse DES.encrypt

Given the following key and ciphertext, find the plaintext using only the solver and the `DES.encrypt` function.

```
known_key = 0x752979387592cb70
known_ct = 0xf2930290ea4db580
```

Note: For whatever reason, the default Z3 solver has trouble with this one. Try one of the other solvers, such as yices:

```bash
labs::CryptoProofs::CryptoProofsAnswers> :s prover=yices
```

Or use all the installed solvers in a first-to-the-post race.
*Caution! May exhaust system resources.*

```bash
labs::CryptoProofs::CryptoProofsAnswers> :s prover=any
```

> Solution:
>
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :s prover=yices
>labs::CryptoProofs::CryptoProofsAnswers> :sat \pt -> DES.encrypt known_key pt == known_ct
>(\pt -> DES.encrypt known_key pt == known_ct)
>  0x70617373776f7264 = True
>(Total Elapsed Time: 1.870s, using "Yices")
>```


#### Exercise 2.1.2 Breaking DES

Given the following matched plaintext and ciphertext, ask the solver to find the key. Will this work? Why or why not? (*Hint: see plaintext.*) Note that you can stop the solver at any time by hitting `ctrl-c`.

```
matched_pt = join "tootough"
matched_ct = 0x95d07f8a72707733
```

To make this solvable, try it again with the first six bytes of key provided: `0x1234567890ab`.

> Solution:
>
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :sat \key -> DES.encrypt key matched_pt == matched_ct
>```
> At this point, the solver hangs, unable to find a solution in any 
> reasonable time. This is because DES is a well-designed cryptographic 
> algorithm and is therefore designed to resist attacks on the key.
> DES keys have been broken using specialized algorithms 
> and large amounts of compute power, but not by a single computer
> running a SAT solver.
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :sat \key -> DES.encrypt (0x1234567890AB # key) matched_pt == matched_ct
>(\key -> DES.encrypt (0x1234567890ab # key)
>                     matched_pt == matched_ct)
>  0x1236 = True
>(Total Elapsed Time: 4.764s, using "Yices")

### 2.2 Proof of Inversion

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
labs::CryptoProofs::CryptoProofsAnswers> :prove \x -> g (f x) == x
Q.E.D.
(Total Elapsed Time: 0.023s, using "Z3")
```

Here's the breadown of this proof:

|Proof of Inversion||||
|-|-|-|-|
| `:prove`            | `\x`                   | `->`        | `g (f x) == x` |
| "Prove to me" | "that for all `x`" | "it is true that" | "`g` inverts `f`" |
|

#### Exercise 2.2.1 The other direction

Our example proof showed that `g` inverts `f` for all inputs. Does this work the other way around? Try it! Why does or doesn't this work?

> Solution:
>
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :prove \x -> f (g x) == x
>(\x -> f (g x) == x) 0 = False
>(Total Elapsed Time: 0.018s, using "Z3")
>```
>
>Here we see that Cryptol has found that not only is our theorem false, 
>but provides a *counterexample* that we can analyze to see why.
>Let's look a little closer.
>
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> g 0
>-1
>```
>
>The reason this doesn't work is because `g` is defined over the integers
>(rather than real numbers, which aren't currently supported by Cryptol.)
>Therefore, the division operator computes integer division, resulting in a
>loss of information for some inputs.

#### Exercise 2.2.2 DES inversion

Use Cryptol to prove that `DES.encrypt` and `DES.decrypt` are inverses for all possible inputs. Show both directions.

*Hint*: Note that this function has more than one input. To make it work, you can either use a tuple `(,)` (tuple-style) or a chain of of `\->` (Curried-style) constructs for your lambda function input.

*Hint*: For fastest results, use the `abc` prover.

>Solution:
>
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :s prover=abc
>labs::CryptoProofs::CryptoProofsAnswers> :prove \(key,pt) -> DES.decrypt key (DES.encrypt key pt) == pt
>Q.E.D.
>(Total Elapsed Time: 3.909s, using "ABC")
>labs::CryptoProofs::CryptoProofsAnswers> :prove \key -> \ct -> DES.encrypt key (DES.decrypt key ct) == ct
>Q.E.D.
>(Total Elapsed Time: 3.582s, using "ABC")
>```

### 2.3 Collision Detection

In cryptography, a *collision* occurs when two different inputs produce the same output. For some cryptographic functions, such as pseudo-random number generators (PRNGs), it may be desirable to demonstrate an absence of collisions. In other functions, such as cryptographic hash functions, collisions are inevitable, but should be difficult to discover. It is easy in Cryptol to ask the solver to search for collisions. (Though finding a solution may not be possible.)

#### Exercise 2.3.1 DES Key Collisions

Use the solver to find two different keys and a plaintext such that both keys encrypt that plaintext to the same ciphertext. *Hint: Try `yices`*.

```bash
labs::CryptoProofs::CryptoProofsAnswers> :sat \(k1,k2,pt) -> k1 != k2 /\ DES.encrypt k1 pt == DES.encrypt k2 pt
(\(k1, k2, pt) -> k1 != k2 /\ DES.encrypt k1 pt == DES.encrypt k2
                                                               pt)
  (0x0000000000000000, 0x0100000000000000, 0x0000000000000000) = True
(Total Elapsed Time: 1.662s, using Yices)
```

#### Exercise 2.3.2 DES Equivalent Keys

It's inevitable that there are collisions in DES, but it may be surprising that they're easy to find with Cryptol's solver. We now know that the two keys you just found encrypt one particular plaintext to the same ciphertext; more concerning would be if they perform the same transformation on *all* plaintexts. Such keys are called *equivalent* keys.

Attempt to prove that the two keys you just found are equivalent keys.

> Solution:
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :prove \pt -> DES.encrypt 0x0000000000000000 pt == DES.encrypt 0x0100000000000000 pt
>Q.E.D.
>(Total Elapsed Time: 0.855s, using Yices)
>```

#### Exercise 2.3.3 DES Parity Bits

Having equivalent keys is often considered a weakness in an a cipher. However, in this case, it turns out that this is a result of a design choice. The lowest bit of each byte of a DES key is actually a *parity* bit that is completely ignored by the cihper itself. The value of the parity bit is such that each byte has an odd number of bits set.

Write a function `DESFixParity : [64] -> [64]` that takes any 64-bit vector and returns the equivalent DES key with the parity bits correctly.

> Solution:
```
DESFixParity : [64] -> [64]
DESFixParity key = join fixed_bytes
  where
    bytes = (split key):[8][8]
    fixed_bytes = [ nibble # [foldl (^) True nibble]
                      where nibble = take`{7} byte 
                  | byte <- bytes ]
```

#### Exercise 2.3.4 DES Key Strength

Use the function `DESFixParity` that you wrote above to show that DES completely ignores parity bits. Given that it does so, what is the actual maximum key strength of DES in terms of bits?

> Solution
>```bash
>labs::CryptoProofs::CryptoProofsAnswers> :prove \(key,pt) -> DES.encrypt key pt == DES.encrypt (DESFixParity key) pt
>Q.E.D.
>(Total Elapsed Time: 1.584s, using Yices)
>```
> Since 8 of the 64 bits are ignored, DES has a maximum key strength of 56 bits.

### 2.4 Equivalence Checking