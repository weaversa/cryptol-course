# Introduction

This lab provides some solutions to common stumbles with Cryptol's
type system.

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in this file.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * Cryptol's type system,
  * manipulating sequences using `#`, `take`, and `reverse`, and `zext`,
  * writing functions and properties,
  * recursion,
  * sequence comprehensions, and
  * logical, comparison, arithmetic, indexing, and conditional
    operators.

## Skills You'll Learn

By the end of this lab you will have a pretty good understanding of
Cryptol's language constructs. At least good enough that you can come
back here for reference as you work through the labs.

Specifically, you'll also gain experience with
  * writing functions and properties,
  * type parameters and type constraints,
  * decimating types through recursion, and
  * [numeric constraint guards](https://galoisinc.github.io/cryptol/master/BasicSyntax.html#numeric-constraint-guards).

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::Language::IntroTypeHackery
Loading module Cryptol
Loading module labs::Language::IntroTypeHackery
```

We start by defining a new module for this lab:

```cryptol
module labs::Language::IntroTypeHackery where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Introductory Type Hackery

Sometimes Cryptol's type system does not provide an intuitive way to
accomplish what appears to be simple. In this document we'll show an
example of this and how it can be overcome.

In a nutshell the problem shown herein (and in general many typing
surprises in Cryptol) stems from the fact that the type system cannot
do any reasoning based on values, rather it must know or deduce the
size of all data at parse time. In some ways this limits us, but it
makes automated reasoning practical and rarely proves to be a
show-stopper for cryptographic algorithms.


## The Exemplar

Following is a typical definition of
[gcd](https://en.wikipedia.org/wiki/Greatest_common_divisor) in
Cryptol:

```cryptol
/**
 * A simply defined recursive gcd.
 */

gcdSimple : {n} (fin n) => [n] -> [n] -> [n]
gcdSimple x y = if y == 0 then x else gcdSimple y (x % y)
```

It's very simple, but it does not symbolically terminate, so that
proofs are impossible when `n` is nonzero. Essentially the symbolic
simulator can't understand that the one or both of the arguments'
values are strictly decreasing in the recursion which would eventually
force the base case. That means the symbolic simulator attempts to
generate an infinite circuit and Cryptol hangs.

In this case, `%` is problematic in that the symbolic simulator allows
for any value to emerge from `%`. There are some operations that cause
the symbolic simulator to realize that some bits have become
fixed. For instance `>> 1` causes the most significant bit of its
argument to be `False`. We can leverage that to express the gcd
function with the binary gcd algorithm in a manner that affords
symbolic termination.


## The Binary gcd Algorithm

The
[binary gcd algorithm](https://en.wikipedia.org/wiki/Binary_GCD_algorithm),
another recursive algorithm for gcd, has the advantage that each
recursion involves a halving of one or both arguments.

## Wikipedia's Description [1]

> 1. gcd(0, _v_) = _v_, because everything divides zero, and _v_ is
>    the largest number that divides _v_. Similarly, gcd(_u_, 0) =
>    _u_. gcd(0, 0) is not typically defined, but it is convenient to
>    set gcd(0, 0) = 0.
> 2. If _u_ and _v_ are both even, then gcd(_u_, _v_) = 2·gcd(_u_/2,
>    _v_/2), because 2 is a common divisor.
> 3. If _u_ is even and _v_ is odd, then gcd(_u_, _v_) = gcd(_u_/2,
>    _v_), because 2 is not a common divisor. Similarly, if _u_ is odd
>    and _v_ is even, then gcd(_u_, _v_) = gcd(_u_, _v_/2).
> 4. If _u_ and _v_ are both odd, and _u_ ≥ _v_, then gcd(_u_, _v_) =
>    gcd((_u_ − _v_)/2, _v_). If both are odd and _u_ < _v_, then
>    gcd(_u_, _v_) = gcd((_v_ − _u_)/2, _u_). These are combinations
>    of one step of the simple Euclidean algorithm, which uses
>    subtraction at each step, and an application of step 3 above. The
>    division by 2 results in an integer because the difference of two
>    odd numbers is even.
> 5. Repeat steps 2–4 until _u_ = _v_, or (one more step) until _u_
>    = 0. In either case, the GCD is 2<sup>_k_</sup>_v_, where _k_ is
>    the number of common factors of 2 found in step 2.

[1] Wikipedia contributors. Binary GCD algorithm
[Internet]. Wikipedia, The Free Encyclopedia; 2020 May 11, 17:27 UTC
[cited 2020 Jul 10]. Available from:
[https://en.wikipedia.org/w/index.php?title=Binary_GCD_algorithm&oldid=956130910]().


## A First Attempt at Binary gcd in Cryptol

```cryptol
bgcd : {n} (fin n) => [n] -> [n] -> [n]
bgcd x y = if x == 0 \/ y == 0 then x ^ y
            | even x /\ even y then bgcd (x >> 1) (y >> 1) << 1
            | even x           then bgcd (x >> 1) y
            |           even y then bgcd x (y >> 1)
            | x >= y           then bgcd ((x - y) >> 1) y
                               else bgcd x ((y - x) >> 1)
```

While this computes correct values, we're still in trouble, as the
subtractions in the last two lines can, as far as the symbolic
simulator is concerned, introduce any value. So, supposing we're part
way through the algorithm and we know that 3 most significant bits of
`x` are `False` and the 5 most significant bis of `y` are `False`, but
`x` and `y` are both odd; then we'll compute either `x - y` or `y - x`
and the symbolic simulator will think we have no knowledge of the bits
of either difference. The subsequent `>> 1` will introduce a single
`False` bit to our computation, but overall we will have decreased the
certainty of the bits in our arguments as we recurse.

The next trick is to explicitly remove the unnecessary bits from the
arguments as we recurse. If we represent halving by trimming a bit
from the argument, the symbolic simulator reckons that one or both of
the arguments eventually have type `[0]` which serves as a base
case. What we have done is change from reducing values to eventually
reach a base case, to reducing the types until the base case is
reached. After that it's all details, but there are several.


## Implementing Binary gcd in Cryptol 2

We'll need a few supporting functions:

+ A test for even is needed for various sized values. It is
  implemented in terms of `odd` below.

```cryptol
/**
 * Indicates whether the argument is even or odd.
 */

even, odd : {n} (fin n) => [n] -> Bit
even = ~odd
odd x = last ([False] # x) // "[False] #" affords type [0]
```

----------

### *Aside*: Numeric Constraint Guards

Update!!!  Cryptol 3 introduced 
[*numeric constraint guards*](https://galoisinc.github.io/cryptol/3.2.0/BasicSyntax.html#numeric-constraint-guards), 
which accommodate different branches based on the type parameters of
a function.  For example, our definitions of `even` and `odd` can be
adapted to:

```cryptol
even', odd' : {n} (fin n) => [n] -> Bit
even' = ~odd'
odd' x
  | n == 0 => False
  | n > 0  => last x
```

Let's add `property`s to verify that these definitions are equivalent...

```cryptol
even_eq, odd_eq : {n} (fin n) => [n] -> Bool
property even_eq = even === even'
property odd_eq = odd === odd'
```

We'll give properties a proper introduction later in the course, but
here's a sneak peek:

```Xcryptol-session
labs::Language::IntroTypeHackery> :prove even_eq`{0}
Q.E.D.
(Total Elapsed Time: 0.026s, using "Z3")
labs::Language::IntroTypeHackery> :prove even_eq`{32}
Q.E.D.
(Total Elapsed Time: 0.030s, using "Z3")
labs::Language::IntroTypeHackery> :prove odd_eq`{0}
Q.E.D.
(Total Elapsed Time: 0.024s, using "Z3")
labs::Language::IntroTypeHackery> :prove odd_eq`{32}
Q.E.D.
(Total Elapsed Time: 0.031s, using "Z3")
```

Z3 says these are equivalent, at least for empty and 32-bit sequences.
Thanks, Z3!  Back to our program...

----------

+ A halving function that also reduces the bit width by 1 if
  possible. This is employed in the recursive call to make the size of
  one or both arguments smaller. It turns out that `take` with the
  right type restrictions does the trick.

```cryptol
/**
 * Halves by discarding the last bit if possible.
 */

halve : {n} (fin n) => [n] -> [max n 1 - 1]
halve = take
```

+ Because the result of a recursive call will be 0 or 1 bit smaller
  than the desired result type, `zext` is applied to all the recursive
  results. In step 2, we also need to double the recursive result
  which is easily achieved by `<< 1`. N.b., in the case where neither
  argument is `0` and both are even, when we recurse the result will
  necessarily be one bit skinnier, `zext` will add back a most
  significant `False` bit and `<< 1` can not cause an overflow.

+ As a convenience, we define an absolute difference function whose
  arguments may be of different bit widths.

```
/**
 * Computes absolute difference with heterotypic arguments.
 */

absDiff : {m, n} (fin m, fin n) => [m] -> [n] -> [max m n]
absDiff x y = if x' > y' then x' - y' else y' - x'
  where
    x', y' : [max m n]
    x' = zext x
    y' = zext y
```

+ Since only one of the arguments may be halved at each recursive
  step, the widths of the two arguments must be independent, giving us
  the more general type signature:

```cryptol
gcd : {m, n} (fin m, fin n) => [m] -> [n] -> [max m n]
```

  We need the result type to be `[max m n]` since if either argument
  is zero, the result is the value of the other argument.

Without further ado, the code:

```cryptol
gcd x y = if x == 0 \/ y == 0 then zext x ^ zext y        // line 1
           | even x /\ even y then zext (gcd x' y') << 1  //      2
           | even x           then zext (gcd x' y )       //      3
           |           even y then zext (gcd x  y')       //      4
           | m' > n'          then zext (gcd z' y )       //      5
           else                    zext (gcd x  z')       //      6
  where
    x' = halve x
    y' = halve x
    z' = halve (absDiff x y)
    m', n' : [max (width m) (width n)]
    m' = `m
    n' = `n
```

Some notation used in the above:

+ `x'`, `y'` and `z'` represent numbers that have been halved and
  shrunk by a bit. They are used as arguments to the recursive call in
  such a way that one or both of the arguments is skinnier.
+ `m'` and `n'` are just the widths of the arguments demoted to the
  value level and sized appropriately so that they may be compared in
  line 5.

Let's discuss the lines above vs. the steps in Wikipedia's algorithm:

+ Line 1 corresponds to step 1. This is the base case in the
  recursion. (Lines 2-6 all have recursive calls.) We compare the
  values to zero rather than the widths as an efficiency improvement
  (and it's slightly easier to write). Of course should a type be
  `[0]` then that argument's value must necessarily be `0`.
+ Line 2 corresponds to step 2.
+ Lines 3-4 correspond to step 3.
+ Lines 5-6 correspond in spirit to step 4. In step 4, we insure that
  the a value decreases. In lines 5-6, we do that, but we also
  insure that the bit width of the wider argument is reduced.

Here are some extra definitions for the courageous.

```
(|?) : {m, n} (fin m, fin n) => [m] -> [n] -> Bit
x |? y = if x == 0 then y == 0 else y' % x' == 0
  where
    x', y' : [max m n]
    x' = zext x
    y' = zext y

gcdDividesBoth : {m, n} (fin m, fin n) => [m] -> [n] -> Bit
property gcdDividesBoth x y = z |? x /\ z |? y
  where
    z = gcd x y

gcdEuclid's : {n} (fin n) => [n] -> [n] -> [n]
gcdEuclid's x y = if x == 0 \/ y == 0 then max x y
                   | x >  y           then gcdEuclid's (x - y) y
                                      else gcdEuclid's  x     (y - x)

gcdCommon : {n} (fin n) => [n] -> [n] -> [n]
gcdCommon x y = if x == 0 \/ y == 0 then max x y
                 | x >  y           then gcdCommon (x % y) y
                                    else gcdCommon  x     (y % x)
```

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [- Language Basics](./Basics.md) ||
|| **Type Hackery** ||
