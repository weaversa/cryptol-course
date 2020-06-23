```
module labs::LanguageBasics::LanguageBasics where
```

# Basic Use of the Cryptol Language

For examples in this lab I have turned off the warning messages you get when not specifying bit sizes of numbers. This is **not** something you should do when you're new at Cryptol. (In fact, I don't do it except when teaching.)

```sh
labs::LanguageBasics::LanguageBasics> :set warnDefaulting = off
```

## Comments

* `//` to end of line
* `/*` ... `*/` block comment

## The Types of Functions

* The Cryptol interpreter command `:t` is very useful for helping you understand types. For instance the type of the `abs` function which we will define later is displayed by:
```sh
labs::LanguageBasics::LanguageBasics> :t abs
abs : Integer -> Integer
```
indicating that it takes an integer and returns an integer.

## Curried and Uncurried Style

Cryptol functions are often written in the [curried] (https://en.wikipedia.org/wiki/Currying) style:

```
gcdCurried: Integer -> Integer -> Integer
```
rather than:
```
gcdUncurried: (Integer, Integer) -> Integer
```
These two functions would be applied as shown:

```sh
labs::LanguageBasics::LanguageBasics> gcdCurried 20 28
4
labs::LanguageBasics::LanguageBasics> gcdUncurried (20, 28)
4
```
These two styles are equivalent at some level. The former is preferred
as it affords [partial application]
(https://en.wikipedia.org/wiki/Partial_application), but the latter
can be useful for explicating the correspondence to functions from other languages or documents. 

* If he helps you, mentally read curried functions as input argument types
  are all the types prior to the last arrow and the type of the result
  is the type after the last arrow. 
* Partial application lets one form a new function from an old one where an argument is fixed.  For instance, `gcdCurried 10` is a function itself! 
    ```sh
    labs::LanguageBasics::LanguageBasics> :t gcdCurried 10
    gcdCurried 10 : Integer -> Integer
    ```
    It takes an integer and returns an integer. When `gcdCurried 10` is applied to an integer it computes the gcd of 10 and that integer. Other examples:
    * Incrementing is addition partially applied to 1. Notionally: `inc x = (add 1) x`
    * The reciprocal is division partially applied to 1. Notionally: `recip x = (div 1) x`
* Really `gcdUncurried` is a function of one argument. That argument is an ordered pair which makes `gcdUncurried (28, 20)` look just like a two argument function in many languages.


## Small Functions

Cryptol programs are just sequences of appropriate functions applied in the correct order. Good Cryptol features small, easy to understand functions composed into conceptually bigger ones. This is good computer science in general, but in Cryptol it is even more advantageous:
* Easy to test—Cryptol's interpreter makes it very cheap to try your functions out.
* Affords easy properties—Properties can be tested easily and, as we'll see in other labs, proven to provide guarantees about code. Moreover, properties serve as another kind of documentation!

### Examples

```
abs : Integer -> Integer
abs n = if n >= 0 then n else -n

abs_nonnegative : Integer -> Bit
property abs_nonnegative x = abs x >= 0
```

* `abs : Integer -> Integer` is the type signature for `abs`
* `abs n = if n >= 0 then n else -n` is the definition for `abs` (or function body)
* `property abs_nonnegative ...` is a property we expect the function to have.
    * `:check property abs_nonnegative` checks this property with random tests. It's super cheap unit testing!
    ```sh
    Main> :check abs_nonnegative 
    Using random testing.
    Passed 100 tests.
    ```
* Cryptol's `if`-`then`-`else` is much like C's ternary operator `?`...`:`. It is not like the `if`-`then`-`else` control structure.
* The reserved word `property` documents that definition's intention.
* Also Cryptol's `:check` with check all functions marked as properties in one go.

A little more involved example:

```
gcd : Integer -> Integer -> Integer
gcd m n = gcd' (abs m) (abs n)
  where
    gcd' : Integer -> Integer -> Integer
    gcd' x y = if y == 0 then x else gcd' y (x % y)

/* This property states that gcd x y is a divisor of both x and y */
gcd_common_divisor' : Integer -> Integer -> Bit
property gcd_common_divisor' x y = x % (gcd x y) == 0 /\ y % (gcd x y) == 0
```
* `where` introduces locally scoped definitions. (Mathematicaians use
  the word "where" in a similar fashion.)
* function `gcd'` is scoped within `gcd`
* function `gcd'` is recursive
* ```sh
    labs::LanguageBasics::LanguageBasics> :check gcd_common_divisor' 
    Using random testing.
    Passed 100 tests.
    ```
* But `gcd_common_divisor' 0 0` gives a division by 0 error.
    ```sh
    labs::LanguageBasics::LanguageBasics> gcd_common_divisor' 0 0
    division by 0
    ```
* We could perhaps have found that with more testing...
    ```sh
    labs::LanguageBasics::LanguageBasics> :set tests=1000
    labs::LanguageBasics::LanguageBasics> :check gcd_common_divisor'
    Using random testing.
    ERROR for the following inputs:
    0
    0
    division by 0
    ```
* Since `:check` uses randomly generated tests the previous result may be intermittent. 
* Properties are useful and later we will actually prove some properties, but you must remember than properties that pass `:check` are not guarantees!
* Properties may be partially applied: `:check gcd_common_divisor' 0` finds the problem faster since it only is using random values for the second argument.

Let's patch up that property. (You surely noticed the prime in the property name which is a giveaway that is not really the property I have in mind.)

```
gcd_common_divisor : Integer -> Integer -> Bit
property gcd_common_divisor x y =
    if z == 0
    then True
    else x % z == 0 /\ y % z == 0
  where
    z = gcd x y
```

* Functions may have many associated properties.
* Properties can have locally scoped definitions. 
* Property corner-cases are handled with `if`-`then`-`else`.



## Writing Loops

### Sometimes you don't have to

* Many of Cryptol's operators naturally extend elementwise over nested sequences to any depth.
```sh
labs::LanguageBasics::LanguageBasics> [[[2,3],[5,7]],[[11,13],[17,19]]] + [[[0,1],[1,2]],[[3,5],[8,13]]]
[[[2, 4], [6, 9]], [[14, 18], [25, 32]]]
```
* So we don't have to write loops within loops to process these sorts of multidimensional arrays.

### Enumerations provide the indices to loops

```sh
Cryptol> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Cryptol> [1, 3..10]
[1, 3, 5, 7, 9]
```

### You can have "infinite" enumerations with `...`

```sh
Cryptol> [1...]
[1, 2, 3, 4, 5, ...]
```
So long as only a finite prefix of any "infinite" calculation is needed we're fine.

### Loops to accumulate a value are often simple calculations over indices

```sh
Cryptol> sum [1..100]
5050
```

### Loops with functions on the indices are written as sequence comprehensions

```sh
Main> [n^^3 | n <- [0 .. 10]]
[0, 1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]
```
* Star Trek's (T.O.S.) warp factor light speed multipliers! 

## Simple Block Encryption Example

```
keyExpand : [32] -> [10][32]
keyExpand key = take roundKeys // take leverages the type signature
  where
    roundKeys : [inf][32]  // a conceptually infinte list
    roundKeys = [key] # [roundKey <<< 1 | roundKey <- roundKeys]

encrypt : [32] -> [32] -> [32]
encrypt key plainText = cipherText
  where
    roundKeys = keyExpand key
    roundResults = [plainText] # [ roundResult ^ roundKey
                                 | roundResult <- roundResults
                                 | roundKey <- roundKeys
                                 ]
    cipherText = last roundResults
```
Many block ciphers are just variations of the above theme. Here's a sample of it in action:
```
labs::LanguageBasics::LanguageBasics> encrypt 0x1337c0de 0xdabbad00 
0x6157c571
labs::LanguageBasics::LanguageBasics> encrypt 0 0xdabbad00 
0xdabbad00
```
Notice you can still write bad crypto with Cryptol!

## Laziness

* Cryptol's evaluation strategy is [lazy] (https://en.wikipedia.org/wiki/Lazy_evaluation) a.k.a. "call-by-need". I.e., computations are not performed until necessary. So

```
lazyAbsMin : Integer -> Integer -> Integer
lazyAbsMin x y = if x == 0 then 0 else min (abs x) (abs y)
```
Does not produce an error when `x` is zero, regardless of the value of `y`. For instance:

```sh
labs::LanguageBasics::LanguageBasics> lazyAbsMin 1 (0/0)

division by 0
labs::LanguageBasics::LanguageBasics> lazyAbsMin 0 (0/0)
0
```

## Signed Operations

Many languages differentiate signed and unsigned numbers at the type level (e.g. C's `uint32` and `int32`). Cryptol has separate operators for signed operations. Most of the time you don't need them as most cryptography works with nonnegative numbers.
* `<$`, `<=$`, `>=$` and `>$` are the signed comparison operators.
* `>>$` is the signed right shift operator, a.k.a. arithmetic right shift
* `/$` and `%$` are signed division and remainder operators

## Don't let the type system do your work

Cryptol's type system tries to infer the types of functions lacking a type signature. Sometimes it comes up with a more general type than you were imagining. This causes problems:
* Perhaps you only want your function to be applicable on a smaller set of types (usually minor, but occasionally major).
* Error messages cab become even more incomprehensible! (Major)

## Do let the type system work for you

Type signatures for functions are wonderful bits of documentation. It is much easier to see what's going on if you use type synonyms and signatures. 

* makes cleaner code
* easier for other tools to consume/reason about

## Supporting code

Following is some code that supports this lab but is not discussed for pedagogical reasons. It does serve to illustrate the type signature and function definitions can be separated within a file. A practice that is **strongly** discouraged. 

```
gcdCurried = gcd  
gcdUncurried = uncurry gcdCurried
```

These illustrate higher order functions. We define the _function_ `gcdCurried` in terms of the _function_ `gcd` without mentioning arguments. We use the built in `uncurry` higher-order function which takes a two argument curried function and returns an uncurried version (a one argument function operating on an ordered pair).
