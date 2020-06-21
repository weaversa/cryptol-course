
# Basic Use of the Cryptol Language

For examples in this lab I have turned off the warning messages you get when not specifying bit sizes of numbers. This is **not** something you should do when you're new at Cryptol. (In fact, I don't do it except when teaching.)

```sh
Cryptol> :set warnDefaulting = off
```

## Comments

* `//` to end of line
* `/*` ... `*/` block comment


## Functions are often written in the [curried] (https://en.wikipedia.org/wiki/Currying) style:

```haskell
gcd: Integer -> Integer -> Integer
```
rather than:
```haskell
gcd2: (Integer, Integer) -> Integer
```

these two would be applied as shown:

```sh
Cryptol> gcd 28 20
4
Cryptol> gcd2 (28, 20)
4
```

These two styles are equivalent at some level. The former is slightly preffered as it affords [partial application] (https://en.wikipedia.org/wiki/Partial_application), but the latter is useful for explicating the correspondence to functions from other languages or documents. 

* Partial application lets one form a new function from an old one where an argument is fixed.  For instance, `gcd 10` is a function itself! When `gcd 10` is applied to an integer it computes the gcd of 10 and that integer. Other examples:
    * Incrementing is addition partially applied to 1. Notionally: `inc x = (add 1) x`
    * The reciprocal is division partially applied to 1. Notionally: `recip x = (div 1) x`
* Really `gcd2` is a function of one argument. That argument is an ordered pair which makes `gcd2 (28, 20)` look just like a two argument function in many languages.


## Small functions

* Good computer science
* Easy to test
* Leverage properties
* 

### Examples

```haskell
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

```haskell
gcd : Integer -> Integer -> Integer
gcd m n = gcd' (abs m) (abs n)
  where
    gcd': Integer -> Integer -> Integer
    gcd' x y = if y == 0 then x else gcd' y (x % y)
    
gcd_common_divisor' : Integer -> Integer -> Bit
property gcd_common_divisor' x y = x % (gcd x y) == 0 /\ y % (gcd x y) == 0
```

* function `gcd'` is scoped within `gcd`
* function `gcd'` is recursive
* ```sh
    Main> :check gcd_common_divisor' 
    Using random testing.
    Passed 100 tests.
    ```
* But `gcd_common_divisor' 0 0` gives a division by 0 error.
* ```sh
    Main> :set tests=1000
    :check gcd_common_divisor'
    Using random testing.
    ERROR for the following inputs:
    0
    0
    division by 0
    ```
* Since `:check` uses randomly generated tests the previous result may be intermittent. 
* Properties are useful and later we will actaully prove some properties, but you must remember than properties that pass `:check` are not guanantees!
* Properties may be partiall applied: `:check gcd_common_divisor' 0` finds the problem faster since it only is using random values for the second argument.

Let's patch up that property. (You surely noticed the prime in the property name which is a giveaway that something's amiss.)

```haskell
gcd_common_divisor : Integer -> Integer -> Bit
property gcd_common_divisor x y = if z == 0
                                  then True
                                  else x % z == 0 /\ y % z == 0
  where
    z = gcd x y
```

* Functions may have many associated properties.
* Properties can have locally scoped defintions. 
* Property corner-cases are handled with `if`-`then`-`else`.



## Writing Loops

### Sometimes you don't have to

* Many of Cryptol's operators naturally extend over nested sequences to any depth.
```sh
Cryptol> [[[2,3],[5,7]],[[11,13],[17,19]]] + [[[0,1],[1,2]],[[3,5],[8,13]]]
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
* So long as only a finite prefix of any "infinite" calculation is needed we're fine.

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

### Simple Block Encryption Example

```haskell
keyexpand : [32] -> [10][32]
keyexpand key = take roundkeys
  where
    roundkeys = [key] # [roundkey <<< 1 | roundkey <- roundkeys]

encrypt : [32] -> [32] -> [32]
encrypt key plaintext = ciphertext
  where
    roundkeys = keyexpand key
    rounds = [plaintext] # [ round ^ roundkey
                           | round <- rounds
                           | roundkey <- roundkeys
                           ]
    ciphertext = last rounds
```


## Laziness



## Signed Comparisons




## Don't let the type system do your work

Cryptol's type system tries to infer the types of functions lacking a type signature. Sometimes it comes up with a more general type than you were imagining. This causes problems:

* Perhaps you only want your function to be applicable on a smaller set of types. (usually minor, but occasionally major)
* Makes error messages even more incomprehensible! (major)

## Do let the type system work for you

Type signatures for functions are wonderful bits of documentation. It is much easier to see what's going on if you use type synonyms and signatures. 

* makes cleaner code
* easier for other tools to consume/reason about

