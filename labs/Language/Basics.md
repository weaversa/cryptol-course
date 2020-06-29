Preface
-------

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document—that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```shell
cryptol> :m labs::Language::Basics
```



Basic Use of the Cryptol Language
=================================


Introduction
------------

In you've programmed in a variety of languages (not just different
[procedural languages](https://en.wikipedia.org/wiki/Procedural_programming)
descending from C), you'll find that, for the most part, Cryptol is
just another language with a different vocabulary and a funky type
system. After a little study you'll be able to do most anything
computationally that you could otherwise, especially in the
cryptographic realm. But once you are accustomed to Cryptol you will
find that it is much easier to write correct cryptographic programs
than with conventional languages. That's 'cause it's been tuned for
such! To throw out the buzzwords:
* Cryptol is a
  [domain-specific language](https://en.wikipedia.org/wiki/Domain-specific_language). Not
  only does it have things to support its domain, but also it elides a
  lot of junk that makes programming and analyzing the programs
  difficult.
* Cryptol has been designed with automated reasoning about its code as
  a priority, so that we can leverage it for verification. Some things
  are harder to do in Cryptol, but they pay off in code that can be
  proven correct!

In some ways this requires a new mindset:
* Write properties about your functions.
* `:check` them.
* Invest in `:prove` when your function's definition has settled down.

Enjoy getting addicted to this level of assurance!


Preliminaries
-------------

The following code declares the module name of this literate Cryptol
document.

```
module labs::Language::Basics where
```

For examples in this lab I have turned off the warning messages you
get when not specifying bit sizes of numbers. This is **not**
something you should do when you're new at Cryptol. (In fact, I don't
do it except when teaching.)

```shell
labs::Language::Basics> :set warnDefaulting = off
```

Also some examples have octets as outputs that are easier to see as
characters so I use:

```shell
labs::Language::Basics> :set ascii = on
```

That makes any sequence of octets be displayed as the corresponding
[ASCII](https://en.wikipedia.org/wiki/ASCII) string in double quotes
(`"`) and an octet outside a sequence be displayed as the
corresponding ASCII character in single quotes (`'`). (This is mostly
useful as a pedagogical aid.)

```shell
labs::Language::Basics> [0x63, 0x61, 0x74]
"cat"
labs::Language::Basics> 0x78
'x'
```

The Cryptol interpreter parses `"abc"` and `[0x61, 0x62, 0x63]` into
the exact same internal representation. `:set ascii = on` just causes
the display of output to be ASCII strings or characters when
appropriate, not unlike using `:set base = 10` to see numbers in
base 10.


Comments
--------

* `//` to end of line
* `/*` ... `*/` block comment

There is also a [docstring](https://en.wikipedia.org/wiki/Docstring)
comment facility:

```
/**
  * A totally made up identifier for pedagogical purposes. It is
  * used elsewhere for demonstration of something or other.
  */
mask = 7
```

Now when issuing `:help mask`, the above comments are displayed along
with other information about `mask`.


Identifiers
-----------

Cryptol identifiers consist of alphanumeric characters plus `'`
(apostrophe, but read "prime") and `_` (underscore). They must begin
with an alphabetic character or an underscore. The notational
convention for `'` is to indicate a related definition, while
underscore is mostly used to separate words or as a catch all for
characters we'd like to use but are
forbidden. [Camel case](https://en.wikipedia.org/wiki/Camel_case) is
often used when other naming constraints aren't mandated.

```
fooBar = 15
fooBar' = fooBar && mask // mask defined elsewhere
```

Technically, Cryptol supports
[Unicode](https://en.wikipedia.org/wiki/Unicode), but for simplicity I
am pretending that it doesn't.


Data
----

Cryptol's "basic" data type is an _n_-dimensional array whose base
type is bits.
* 0-d: `False : Bit` and `True: Bit`
* 1-d: Think bytes, words, nibbles, etc., i.e., a sequence of bits of
  any length usually thought of as a number. E.g., `0x2a : [8]`,
  `0b101010 : [6]` and
  `[False, True, False, True, False, True, False] : [7]`. These all
  compare as 42 in type appropriate contexts.
* 2-d: Think sequences of 1-d objects all of the same size. E.g.,
  `[42, 0b010101010101, 0xa5a, 0o5757] : [4][12]`
* 3-d: Sequences of 2-d objects all of the same size. E.g.,
  `[[0, 1], [1, 2], [3, 5], [8, 13]] : [4][2][4]`
* ...

Things to note:
* There are no privileged bit widths in Cryptol. `[13]` is just as
  good a type as `[8]`, `[16]`, `[32]` or `[64]`.
* There's `0b...` for binary, `0o...` for octal and `0x...` for
  hexadecimal.
* Lengths of sequences may be zero. Zero length sequences act as an
  identity for concatenation and are useful in padding.
* The possible values by type:
  * `[0]`—`0`
  * `[1]`—`0` and `1`
  * `[2]`—`0`, `1`, `2` and `3`
  * ...
  * `[n]`—`0` through `2^^n - 1`

  There are 2<sup>_n_</sup> values of type `[n]`. There are
  2<sup>_mn_</sup> values of type `[m][n]`, etc.
* 1-d sequences of bits are treated as numbers by arithmetic and
  comparison operators. So for instance, `[False, True] == (1 : [2])`
  and `[True, False, True] > 4` both hold.
* Cryptol distinguishes between the different dimensions. In
  particular, `True` and `[True]` are type incompatible.
* The number of bracket pairs in a type gives its dimension.

Other data types include:
* Arbitrary-precision integers: E.g., `2^^1023 - 347835479 : Integer`
* Heterogeneous tuples: E.g.: `(False, 0b11) : (Bit, [2])` and
  `(True, [1, 0], 7625597484987) : (Bit, [2][1], Integer)`
  * Elements of tuples are accessed by `.0`, `.1`, ...
    ```shell
    labs::Language::Basics> (False, 0b11).0
    False
    ```
* Records with named fields: E.g.,
  `{flag = True, x = 2} : {flag : Bit, x : [4]}`
  * Elements of records are accessed by `.` followed by the field name.
    ```shell
    labs::Language::Basics> {flag = True, x = 2}.flag
    True
    ```
* Integers modulo _n_: Types of the form `[n]` already provide
  [least residue systems](https://en.wikipedia.org/wiki/Modular_arithmetic#Residue_systems)
  for
  [integers modulo 2<sup>n</sup>](https://en.wikipedia.org/wiki/Modular_arithmetic#Integers_modulo_n).
  Types of the form `Z n` provide that for any positive _n_. E.g.,
  `4 + 4 : Z 7` evaluates to `1`.


Operators
---------

Cryptol's `:help` command will provide a brief description of the
operators in this section by issuing `:help ` followed
by the name of the operator in parentheses. For example: `:help (@)`

Many languages differentiate signed and unsigned numbers at the type
level (e.g. C's `uint32` and `int32`). Cryptol has separate operators
for signed operations which are indicated by a suffixed `$`. Most of
the time you don't need them as cryptography tends to use nonnegative
numbers.

Where appropriate, operators act elementwise (or "blast through")
typing constructs like sequences, tuples and records.

```shell
labs::Language::Basics> [[0, 1], [1, 2]] + [[3, 5], [8, 13]]
[[3, 6], [9, 15]]
labs::Language::Basics> (3, (1, 4)) + (1, (5, 9))
(4, (6, 13))
labs::Language::Basics> {x = 1, y = 3} + {y = 6, x = 10}
{x = 11, y = 9}
labs::Language::Basics> [(0, 1), (4, 9), (16, 25)].1
[1, 9, 25]
labs::Language::Basics> [{x = 1, y = 3}, {y = 6, x = 10}].y
[3, 6]
```

Following are some really quick examples of operators to remind you
and show some tricks of Cryptol.

### Arithmetic: `+`, `-`, `*`, `/`, `%` and `^^`
#### Signed versions: `/$` and `%$`

```shell
labs::Language::Basics> 1 + 1
2
labs::Language::Basics> 1 + 1 : [1]
0x0
labs::Language::Basics> 2^^127 - 1 // a 33 digit Mersenne prime
170141183460469231731687303715884105727
```

The first example defaults to type `Integer`. In the second, I
explicitly state that I want 1 bit addition and so the computation is
modular addition. The third shows that `^^` is exponentiation

The division (`/`) operation is not what a mathematician imagines in
modular arithmetic. For instance `3 * 3 == 1 : [3]` so a mathematician
would expect `1 / 3 == 3 : [3]` since division is the inverse of
multiplication. However, in Cryptol `1 / 3 == 0 :
[3]`. Mathematically, `/` and `%` yield the quotient and remainder,
respectively, in
[Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division).

### Bitwise logical: `~`, `&&`, `||` and `^`

```shell
labs::Language::Basics> ~0b000011001101 && 0o4115 || 0x0d0 ^ 9
0x8d9
```


### Comparison:`==`, `!=`, `<` , `<=`, `>` and `>=`
#### Signed versions: `<$`, `<=$`, `>$` and `>=$`

```shell
labs::Language::Basics> [~1, 1] == [6 : [3], 3 * 3]
True
labs::Language::Basics> [~1, 1] == [6 : [4], 3 * 3]
False
```

In the first example, `6` is the literal needing the most bits and is
given type `[3]`. That makes both sides of the equality test have type
`[2][3]` (two elements of three bits each). Now `~1 : [3]` is `0b110`
or `6` in decimal and `3 * 3 : [3]` is `1 : [3]` so `[~1, 1] == [6, 1]
== [6, 3 * 3]`.

In the second example, `6` is given type `[4]`, so both sides have
type `[2][4]`. Now `~1 : [4]` is `0b1110` or `14` in decimal and
`3 * 3 : [4]` is `9` in decimal. We have `[~1, 1] == [14, 1]`
while `[6, 3 * 3] == [6, 9]` so equality fails.

_**It can be crucially important to be precise about the widths of
things!**_

Comparisons are lexicographic on sequences of numbers.

```shell
labs::Language::Basics> [1, 2] < [1, 3]
True
labs::Language::Basics> [1, 2] < [1, 2]
False
```

### Shifts: `<<`, `>>`, `<<<` and `>>>`
#### Signed version: `>>$`

```shell
labs::Language::Basics> 0xa5a << 4
0x5a0
labs::Language::Basics> 0xa5a << 12
0x000
labs::Language::Basics> 0xa5a <<< 16
0x5aa
```

### Indexing and slicing: `@`, `!`, `@@` and `!!`

```shell
labs::Language::Basics> "cat" @ 0
'c'
labs::Language::Basics> "dog" @@ [2, 1, 1, 0, 0, 1, 2]
"gooddog"
labs::Language::Basics> "cow" ! 0
'w'
```

Notice that these operators all use 0-based indexing: `@` and `@@`
from the beginning of the sequence and `!` and `!!` from the end.

### Concatenation: `#`

```shell
labs::Language::Basics> "dog" # "cow" // Moof!
"dogcow"
```

### Shortcutting logical: `/\`, `\/` and `==>`

These are most often used in property statements. `/\` is "and", `\/`
is "or" and `==>` is "implies". They have very low precedence.

```shell
labs::Language::Basics> 1 == 5 \/ 5 == 5
True
labs::Language::Basics> False ==> 1 == 5 /\ 1 != 5
True
```


Common Primitives
-----------------

Cryptol's `:help` command will provide a brief description of the
primitives in the section by issuing `:help ` followed
by the name of the primitive.

### Collections of all `False` or all `True` bits

* `0` is a sequence of `False` bits whose type is determined by the
context.
  ```shell
  labs::Language::Basics> 0 : [12]
  0x000
  ```
* `zero` is an arbitrary collection of `False` bits whose type
is determined by the context.
  ```shell
  labs::Language::Basics> zero: ([8], [4])
  (0x00, 0x0)
  ```
  Here we produce an order pair of a 0 octet and a 0 nibble.
* `~0` and `~zero` produce all `True` bits correspondingly.


### List manipulation: `take`, `drop`, `tail`, `last` and `reverse`
```
labs::Language::Basics> take "dogcow" : [3][8]
"dog"
labs::Language::Basics> drop [2, 3, 5, 7, 11] : [3]Integer
[5, 7, 11]
labs::Language::Basics> tail [0, 1, 1]
[1, 1]
labs::Language::Basics> last [2, 3, 5, 7, 11]
11
labs::Language::Basics> reverse [0, 0, 1]
[1, 0, 0]
```

Of course the sizes of lists have to be big enough. Also, notice that
`last` (which is equivalent to `! 0`) returns an element while the
others return lists.

Often in a Cryptol program, the context will determine the shapes of
sequences, so that the type annotations (`: [3][8]` and `: [3]Integer`
above) will be unnecessary.

### List shape manipulation: `split`, `join`, `transpose`
```shell
labs::Language::Basics> split 0xdeadbeef : [8][4]
[0xd, 0xe, 0xa, 0xd, 0xb, 0xe, 0xe, 0xf]
labs::Language::Basics> join [0xca, 0xfe]
0xcafe
labs::Language::Basics> transpose [[1, 2], [3, 4]]
[[1, 3], [2, 4]]
```

In most Cryptol programs, the context will enforce the size of things,
so the type annotations shown in these examples need not be present.




The Types of Functions
----------------------

The Cryptol interpreter command `:type` is very useful for helping you
understand types. For instance the type of the `abs` function which we
will define later is displayed by:

```shell
labs::Language::Basics> :type abs
abs : Integer -> Integer
```

indicating that it takes an integer and returns an integer.


Curried and Uncurried Style
---------------------------

Cryptol functions are often written in the
[curried](https://en.wikipedia.org/wiki/Currying) style:

```
gcdCurried: Integer -> Integer -> Integer
```

rather than:

```
gcdUncurried: (Integer, Integer) -> Integer
```

These two functions would be applied as shown:

```shell
labs::Language::Basics> gcdCurried 20 28
4
labs::Language::Basics> gcdUncurried (20, 28)
4
```

These two styles are equivalent at some level. The former is preferred
as it affords
[partial application](https://en.wikipedia.org/wiki/Partial_application),
but the latter can be useful for explicating the correspondence to
functions from other languages or documents.

* If it helps you, mentally read curried functions like this: input
  argument types are all prior to the last arrow and the
  result type follows the last arrow. Pictorially: `in -> in -> ... ->
  in -> out`
* Partial application lets one form a new function from an old one
  where an argument is fixed.  For instance, `gcdCurried 10` is a
  function itself!
  ```shell
  labs::Language::Basics> :type gcdCurried 10
  gcdCurried 10 : Integer -> Integer
  ```
  `gcdCurried 10` takes an integer and returns an integer. When it
  is applied to an integer it computes the gcd of 10 and that
  integer. Other examples to illustrate partial application:
  * Incrementing is addition partially applied to 1. Notionally:
    `inc x = (add 1) x`
  * The reciprocal is division partially applied to 1. Notionally:
    `recip x = (div 1) x`
* Really `gcdUncurried` is a function of one argument. That argument
  is an ordered pair which makes `gcdUncurried (28, 20)` look just
  like a two argument function in many languages.


Small Functions
---------------

Cryptol programs are just sequences of appropriate functions applied
in the correct order. Good Cryptol features small, easy to understand
functions composed into conceptually bigger ones. This is good
computer science in general, but in Cryptol it is even more
advantageous:
* Easy to test—Cryptol's interpreter makes it very cheap to try your
  functions out.
* Encourages programming with properties—Properties can be tested
  easily and, as we'll see, proven to provide guarantees about
  code. Moreover, properties serve as another kind of documentation!

### Examples

```
abs : Integer -> Integer
abs n = if n >= 0 then n else -n

absNonnegative : Integer -> Bit
property absNonnegative x = abs x >= 0
```

* `abs : Integer -> Integer` is the type signature for `abs`.
* `abs n = if n >= 0 then n else -n` is the definition for `abs` (or function body).
* `property absNonnegative ...` is a property we expect the function to have.
* `:check property absNonnegative` checks this property with
    random tests. It's super cheap unit testing!
  ```shell
  labs::Language::Basics> :check absNonnegative
  Using random testing.
  Passed 100 tests.
  ```
* Cryptol's `if ... then ... else` is much like C's ternary operator
  `?`...`:`. It is not like the `if ... then ... else` control structure.
* The reserved word `property` documents that definition's intention.
* We can go a step further and `:prove` this property:
  ```shell
  labs::Language::Basics> :prove absNonnegative
  Q.E.D.
  (Total Elapsed Time: 0.032s, using Z3)
  ```
* Also Cryptol's `:check` will check all functions marked as
  properties in one go and, you guessed it, `:prove` works similarly.

A little more involved example follows.

```
gcd : Integer -> Integer -> Integer
gcd m n = gcd' (abs m) (abs n)
  where
    gcd' : Integer -> Integer -> Integer
    gcd' x y = if y == 0 then x else gcd' y (x % y)

/* This property states that gcd x y is a divisor of both x and y */
gcdDividesBoth' : Integer -> Integer -> Bit
property gcdDividesBoth' x y
    =  x % (gcd x y) == 0
    /\ y % (gcd x y) == 0
```

* `where` introduces locally scoped definitions. (Mathematicians use
  the word "where" in a similar fashion.)
* The function `gcd'` is scoped within `gcd`.
* The function `gcd'` is recursive.
* Let's check `gcdDividesBoth'`:
  ```shell
  labs::Language::Basics> :check gcdDividesBoth'
  Using random testing.
  Passed 100 tests.
  ```
* It seems okay, yet `gcdDividesBoth' 0 0` gives a division by 0 error.
  ```shell
  labs::Language::Basics> gcdDividesBoth' 0 0
  division by 0
  ```
* We could perhaps have found that with more testing:
  ```shell
  labs::Language::Basics> :set tests=1000
  labs::Language::Basics> :check gcdDividesBoth'
  Using random testing.
  ERROR for the following inputs:
  0
  0
  division by 0
  ```
* Since `:check` uses randomly generated tests the failing result may
  be intermittent.
* Properties are useful and sometimes may be `:prove`-n, but you must
  remember that _**properties that pass `:check` are not
  guarantees!**_ That is: _**`:check` is evidence, `:prove` is
  proof!**_
* Properties may be partially applied: `:check gcdDividesBoth' 0`
  finds the problem faster since it only is using random values for
  the second argument.
* **Warning**: `:prove gcdDividesBoth'` will never complete. If you
  issue that command, you'll need to issue the abort sequence (often
  `Control-C`) once or twice to interrupt and regain control. The
  reason this proof won't complete is too technical for the moment.

Let's patch up that property. (You surely noticed the prime (`'`) in
the property name which is a giveaway that is not quite the property
I have in mind.)

```
gcdDividesBoth : Integer -> Integer -> Bit
property gcdDividesBoth x y
    = if z == 0
       then True
       else x % z == 0 /\ y % z == 0
  where
    z = gcd x y
```

* Functions may have many associated properties.
* Properties can have locally scoped definitions.
* Property corner-cases are handled with `if ... then ... else`.
* Another way to write the conditional part of the property that's a
  bit cooler: `z != 0 ==> x % z == 0 /\ y % z == 0`. You can read that
  as, "nonzero _z_ implies _z_ divides both _x_ and _y_".
* WARNING: `:prove gcdDividesBoth` will never complete. Don't try it
  unless your favorite key combination is the abort sequence (often
  `Control-C`) on your computer.


Writing Loops
-------------

### Or not...

* Many of Cryptol's operators naturally extend elementwise over nested
  sequences to any depth.
```shell
labs::Language::Basics> [[[2,3],[5,7]],[[11,13],[17,19]]] + [[[0,1],[1,2]],[[3,5],[8,13]]]
[[[2, 4], [6, 9]], [[14, 18], [25, 32]]]
```

* So we don't have to write loops within loops to process these sorts
of multidimensional arrays.
* All the arithmetic, bitwise logical and comparison operators work
  elementwise over nested sequences!

### Loop indices

Enumerations serve to provide the indices to loops.

```shell
labs::Language::Basics> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
labs::Language::Basics> [1, 3..10]
[1, 3, 5, 7, 9]
```

#### Infinite indices

You can have "infinite" enumerations with `...`.

```shell
labs::Language::Basics> [1...]
[1, 2, 3, 4, 5, ...]
```

So long as only a finite prefix of any "infinite" calculation is needed we're fine.

### Loops to accumulate a value

Loops to accumulate a value are often simple calculations over indices.

```shell
labs::Language::Basics> sum [1..100]
5050
```

### Loops with functions on the indices

Loops with functions on the indices are written as sequence comprehensions.

```shell
labs::Language::Basics> [n^^3 | n <- [0..10]]
[0, 1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]
```

Star Trek's (T.O.S.) warp factor light speed multipliers!

### Loops that modify an accumulator in place

Loops that modify an accumulator in place become self-referential
sequence comprehensions. The following example illustrates this.



Simple Block Encryption Example
-------------------------------

```
keyExpand : [32] -> [10][32]
keyExpand key = take roundKeys // take leverages the type signature
  where
    roundKeys : [inf][32]  // a conceptually infinite list
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

```shell
labs::Language::Basics> encrypt 0x1337c0de 0xdabbad00
0x6157c571
labs::Language::Basics> encrypt 0 0xdabbad00
0xdabbad00
```

The latter shows that you can still write bad crypto with Cryptol! 😉

Notice that both `roundKeys` in `keyExpand` and `roundResults` in
`encrypt` are self-referential sequences, a paradigm that will often
occur when coding up cryptography.


Laziness
--------

Cryptol's evaluation strategy is
[lazy](https://en.wikipedia.org/wiki/Lazy_evaluation)
a.k.a. "call-by-need". I.e., computations are not performed until
necessary. So
```
lazyAbsMin : Integer -> Integer -> Integer
lazyAbsMin x y = if x == 0 then 0 else min (abs x) (abs y)
```
Does not produce an error when `x` is zero, regardless of the value of `y`. For instance:
```shell
labs::Language::Basics> lazyAbsMin 1 (0/0)

division by 0
labs::Language::Basics> lazyAbsMin 0 (0/0)
0
```


Less Common Operators
---------------------

Function equality: `===` and `!==`. These are mostly used to state
properties about functions over a finite domain.

```
add8 : [4] -> [4]
add8 x = x + 8
sub8 : [4] -> [4]
sub8 x = x - 8
```

```shell
labs::Language::Basics> :prove add8 === sub8
Q.E.D.
(Total Elapsed Time: 0.014s, using Z3)
```

There is also an `:exhaust` command for finite domains. On occasion
the machinery behind `:prove` gets overwhelmed and, on small enough
domains, exhausting works in a reasonable time.

```shell
labs::Language::Basics> :exhaust add8 === sub8
Using exhaustive testing.
Passed 16 tests.
Q.E.D.
```

The `:check` command is smart enough to notice small enough domains
and switch to exhaustion automagically:

```shell
labs::Language::Basics> :check add8 === sub8
Using exhaustive testing.
Passed 16 tests.
Q.E.D.
```


Judicious Type System Usage
---------------------------

### Don't let the type system do your work

Cryptol's type system tries to infer the types of functions lacking a
type signature. Sometimes it comes up with a more general type than
you were imagining. This causes problems:
* Perhaps you only want your function to be applicable on a smaller
  set of types (usually minor, but occasionally major).
* Error messages can become even more incomprehensible! (Major)

### Do let the type system work for you

Type signatures for functions are wonderful bits of documentation. It
is much easier to see what's going on if you use type synonyms and
signatures.

* cleaner code
* easier for other tools to consume/reason about

### Provide additional types to aid in debugging

Many of the errors in coding Cryptol will be instances of type
mismatching. If you can't see your problem based on the error message,
try adding more type annotations. This:
* makes the interpreter do less work trying alternative possibilities
and, consequently, can make error messages more comprehensible
* reduces the body of code to examine for bugs (a sort of binary bug
search)
* can get you to notice where you blew it


Here Abide Monsters
-------------------

Following is some code that is needed to make the Cryptol within this
document valid, but is not discussed for pedagogical reasons. You may
read on, but you have been warned...

The following code does serve to illustrate the type signature and
function definitions can be separated within a file. A practice that
is _**strongly**_ discouraged.

```
gcdCurried = gcd
gcdUncurried = uncurry gcdCurried
```

These also illustrate higher order functions. We define the _function_
`gcdCurried` in terms of the _function_ `gcd` without mentioning
arguments. We use the built in `uncurry` higher-order function which
takes a two argument curried function and returns an uncurried version
(a one argument function operating on an ordered pair).


Postface
--------

Go forth and write correct cryptographic algorithms!
