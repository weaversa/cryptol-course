# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```shell
cryptol> :m labs::LanguageBasics::LanguageBasics
```



```
module labs::LanguageBasics::LanguageBasics where
```



Basic Use of the Cryptol Language
=================================

For examples in this lab I have turned off the warning messages you
get when not specifying bit sizes of numbers. This is **not**
something you should do when you're new at Cryptol. (In fact, I don't
do it except when teaching.)

```sh
labs::LanguageBasics::LanguageBasics> :set warnDefaulting = off
```

Also some examples have eight bit outputs that are easier to see as
characters so I use:

```sh
labs::LanguageBasics::LanguageBasics> :set ascii = on
```

That makes any sequence of octets be displayed as the corresponding
ASCII string in double quotes (`"`) and an octet outside a sequence be
displayed as the corresponding ASCII character in single quotes
(`'`). (This is mostly useful as a pedagogical aid.)

```sh
labs::LanguageBasics::LanguageBasics> [0x63, 0x61, 0x74]
"cat"
labs::LanguageBasics::LanguageBasics> 0x78
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
mask = 7
fooBar = 15
fooBar' = fooBar && mask
```

Technically, Cryptol supports
[Unicode](https://en.wikipedia.org/wiki/Unicode), but for simplicity I
am pretending that it doesn't.


Data
----

Cryptol's "basic" data type is an _n_-dimensional array whose base
type is bits.
* 0-d: `False : Bit` and `True: Bit`
* 1-d: Think bytes, words, nibbles, etc., i.e., a list of bits usually
  thought of as a number. E.g., `0xaa : [8]` and `0b1011 : [4]`
* 2-d: Think lists of 1-d objects all of the same size. E.g.,
  `[42, 0b010101010101, 0xa5a, 0o5757] : [4][12]`
* 3-d: Lists of 2-d objects all of the same size. E.g.,
  `[[0, 1], [1, 2], [3, 5], [8, 13]] : [4][2][4]`
* ...
  
Other data types include:
* Arbitrary-precision integers: E.g., `2^^1023 - 347835479 : Integer`
* Heterogeneous tuples: E.g.: `(False, 0b11) : (Bit, [2])` and
  `(True, [1, 0], 7625597484987) : (Bit, [2][1], Integer)`
* Records with named fields: E.g.,
  `{flag = True, x = 2} : {flag : Bit, x : [4]}`


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

Following are some really quick examples of operators to remind you
and show some tricks of Cryptol.

### Arithmetic: `+`, `-`, `*`, `/`, `%` and `^^`
#### Signed versions: `/$` and `%$`

```sh
labs::LanguageBasics::LanguageBasics> 1 + 1
2
labs::LanguageBasics::LanguageBasics> 1 + 1 : [1]
0x0
labs::LanguageBasics::LanguageBasics> 2^^127 - 1 // a 33 digit Mersenne prime
170141183460469231731687303715884105727
```

The first example defaults to type `Integer`. In the second, I
explicitly state that I want 1 bit addition. The third shows that `^^`
is exponentiation


### Bitwise logical: `~`, `&&`, `||` and `^`

```sh
labs::LanguageBasics::LanguageBasics> ~0b000011001101 && 0o4115 || 0x0d0 ^ 9
0x8d9
```

Notice there's `0b...` for binary, `0o...` for octal and `0x...` for hexadecimal.

### Comparison:`==`, `!=`, `<` , `<=`, `>` and `>=`
#### Signed versions: `<$`, `<=$`, `>$` and `>=$`

```sh
labs::LanguageBasics::LanguageBasics> [~1, 1] == [6, 4 + 5]
True
labs::LanguageBasics::LanguageBasics> [~1, 1] == [6, 0b0100 + 5]
False
```

Cryptol figures that the `6` in the first example requires three bits
and that's the widest thing so both sides are of type `[2][3]` (two
elements of three bits each). So `[~1, 1] == [6, 1] == [6, 4 + 5]`.

The `0b0100` in the second example needs four bits, so both sides have
type `[2][4]`. In this case `[~1, 1] == [14, 1]` while `[6, 4 + 5] ==
[6, 9]` so equality fails.

_**It is important to be precise about the widths of things!**_

Comparisons are lexicographic on sequences of numbers.

```sh
labs::LanguageBasics::LanguageBasics> [1, 2] < [1, 3]
True
labs::LanguageBasics::LanguageBasics> [1, 2] < [1, 2]
False
```

### Shifts: `<<`, `>>`, `<<<` and `>>>`
#### Signed version: `>>$`

```sh
labs::LanguageBasics::LanguageBasics> 0xa5a << 4
0x5a0
labs::LanguageBasics::LanguageBasics> 0xa5a << 12
0x000
labs::LanguageBasics::LanguageBasics> 0xa5a <<< 16
0x5aa
```

### Indexing and slicing: `@`, `!`, `@@` and `!!`

```sh
labs::LanguageBasics::LanguageBasics> "cat" @ 0
'c'
labs::LanguageBasics::LanguageBasics> "dog" @@ [2, 1, 1, 0, 0, 1, 2]
"gooddog"
labs::LanguageBasics::LanguageBasics> "cow" ! 0
'w'
```

Notice that these operators all use 0-based indexing: `@` and `@@`
from the front of the list and `!` and `!!` from the back.

### Concatenation: `#`

```sh
labs::LanguageBasics::LanguageBasics> "dog" # "cow" // Moof!
"dogcow"
```

### Shortcutting logical:: `/\`, `\/` and `==>`

These are most often used in property statements. `/\` is "and", `\/` is
"or" and `==>` is "implies". They have very low precedence.

```sh
labs::LanguageBasics::LanguageBasics> 1 == 5 \/ 5 == 5
True
labs::LanguageBasics::LanguageBasics> False ==> 1 == 5 /\ 1 != 5
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
  ```sh
  labs::LanguageBasics::LanguageBasics> 0 : [12]
  0x000
  ```
* `zero` is an arbitrary collection of `False` bits whose type
is determined by the context.
  ```sh
  labs::LanguageBasics::LanguageBasics> zero: ([8], [4])
  (0x00, 0x0)
  ```
  Here we produce an order pair of a 0 octet and a 0 nibble.
* `~0` and `~zero` produce all `True` bits correspondingly.


### List manipulation: `take`, `drop`, `tail`, `last` and `reverse`
```
labs::LanguageBasics::LanguageBasics> take "dogcow" : [3][8]
"dog"
labs::LanguageBasics::LanguageBasics> drop`{2} [2, 3, 5, 7, 11]
[5, 7, 11]
labs::LanguageBasics::LanguageBasics> tail [0, 1, 1]
[1, 1]
labs::LanguageBasics::LanguageBasics> last [2, 3, 5, 7, 11]
11
labs::LanguageBasics::LanguageBasics> reverse [0, 0, 1]
[1, 0, 0]
```

Of course the sizes of lists have to be big enough. Also, notice that
`last` (which is equivalent to `!0`) returns an element while the
others return lists.

Often in a Cryptol program, the context will determine the shapes of
sequences, so that the type annotations (`: [3][8]` and `` `{2} ``
above) will be unnecessary.

### List shape manipulation: `split`, `join`, `transpose`
#### Variation: `groupBy` 
```sh
labs::LanguageBasics::LanguageBasics> split 0xdeadbeef : [8][4]
[0xd, 0xe, 0xa, 0xd, 0xb, 0xe, 0xe, 0xf]
labs::LanguageBasics::LanguageBasics> join [0xca, 0xfe]
0xcafe
labs::LanguageBasics::LanguageBasics> transpose [[1, 2], [3, 4]]
[[1, 3], [2, 4]]
labs::LanguageBasics::LanguageBasics> groupBy`{12} 0x5c00b1 // group into 12 bit chunks 
[0x5c0, 0x0b1]
```

In most Cryptol programs, the context will enforce the size of things,
so the type annotations shown in these examples need not be present.




The Types of Functions
----------------------

* The Cryptol interpreter command `:type` is very useful for helping you
  understand types. For instance the type of the `abs` function which
  we will define later is displayed by:
  
```sh
labs::LanguageBasics::LanguageBasics> :type abs
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

```sh
labs::LanguageBasics::LanguageBasics> gcdCurried 20 28
4
labs::LanguageBasics::LanguageBasics> gcdUncurried (20, 28)
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
  ```sh
  labs::LanguageBasics::LanguageBasics> :type gcdCurried 10
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
  easily and, as we'll see in other labs, proven to provide guarantees
  about code. Moreover, properties serve as another kind of
  documentation!

### Examples

```
abs : Integer -> Integer
abs n = if n >= 0 then n else -n

absNonnegative : Integer -> Bit
property absNonnegative x = abs x >= 0
```

* `abs : Integer -> Integer` is the type signature for `abs`
* `abs n = if n >= 0 then n else -n` is the definition for `abs` (or function body)
* `property absNonnegative ...` is a property we expect the function to have.
* `:check property absNonnegative` checks this property with
    random tests. It's super cheap unit testing!
  ```sh
  labs::LanguageBasics::LanguageBasics> :check absNonnegative 
  Using random testing.
  Passed 100 tests.
  ```
* Cryptol's `if ... then ... else` is much like C's ternary operator
  `?`...`:`. It is not like the `if ... then ... else` control structure.
* The reserved word `property` documents that definition's intention.
* We can go a step further and `:prove` this property:
  ```sh
  labs::LanguageBasics::LanguageBasics> :prove absNonnegative 
  Q.E.D.
  (Total Elapsed Time: 0.032s, using Z3)
  ```
* Also Cryptol's `:check` with check all functions marked as
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
* function `gcd'` is scoped within `gcd`
* function `gcd'` is recursive
* ```sh
  labs::LanguageBasics::LanguageBasics> :check gcdDividesBoth' 
  Using random testing.
  Passed 100 tests.
  ```
* But `gcdDividesBoth' 0 0` gives a division by 0 error.
  ```sh
  labs::LanguageBasics::LanguageBasics> gcdDividesBoth' 0 0
  division by 0
  ```
* We could perhaps have found that with more testing:
  ```sh
  labs::LanguageBasics::LanguageBasics> :set tests=1000
  labs::LanguageBasics::LanguageBasics> :check gcdDividesBoth'
  Using random testing.
  ERROR for the following inputs:
  0
  0
  division by 0
  ```
* Since `:check` uses randomly generated tests the previous result may
  be intermittent.
* Properties are useful and in other labs we will actually `:prove` some
  properties, but you must remember than **properties that pass `:check`
  are not guarantees!**
* Properties may be partially applied: `:check gcdDividesBoth' 0`
  finds the problem faster since it only is using random values for
  the second argument.
* WARNING: `:prove gcdDividesBoth'` will never complete. If you issue
  that command, you'll need to issue the abort sequence (often
  `Control-C`) once or twice to interrupt and regain control. The
  reason this proof won't complete is too technical for the moment.
  
Let's patch up that property. (You surely noticed the prime (`'`) in
the property name which is a giveaway that is not really the property
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
bit cooler: `z != 0 ==> x % z == 0 /\ y % z == 0`
* WARNING: `:prove gcdDividesBoth` will never complete. Don't try it
  unless your favorite key combination is the abort sequence (often
  `Control-C`) on your computer.


Writing Loops
-------------

### Or not...

* Many of Cryptol's operators naturally extend elementwise over nested
  sequences to any depth.
```sh
labs::LanguageBasics::LanguageBasics> [[[2,3],[5,7]],[[11,13],[17,19]]] + [[[0,1],[1,2]],[[3,5],[8,13]]]
[[[2, 4], [6, 9]], [[14, 18], [25, 32]]]
```

* So we don't have to write loops within loops to process these sorts
of multidimensional arrays.
* All the arithmetic, bitwise logical and comparison operators work
  elementwise over nested sequences!

### Loop indices

Enumerations serve to provide the indices to loops.

```sh
labs::LanguageBasics::LanguageBasics> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
labs::LanguageBasics::LanguageBasics> [1, 3..10]
[1, 3, 5, 7, 9]
```

#### Infinite indices

You can have "infinite" enumerations with `...`.

```sh
labs::LanguageBasics::LanguageBasics> [1...]
[1, 2, 3, 4, 5, ...]
```

So long as only a finite prefix of any "infinite" calculation is needed we're fine.

### Loops to accumulate a value

Loops to accumulate a value are often simple calculations over indices.

```sh
labs::LanguageBasics::LanguageBasics> sum [1..100]
5050
```

### Loops with functions on the indices

Loops with functions on the indices are written as sequence comprehensions.

```sh
labs::LanguageBasics::LanguageBasics> [n^^3 | n <- [0 .. 10]]
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

```sh
labs::LanguageBasics::LanguageBasics> encrypt 0x1337c0de 0xdabbad00 
0x6157c571
labs::LanguageBasics::LanguageBasics> encrypt 0 0xdabbad00 
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
```sh
labs::LanguageBasics::LanguageBasics> lazyAbsMin 1 (0/0)

division by 0
labs::LanguageBasics::LanguageBasics> lazyAbsMin 0 (0/0)
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

```sh
labs::LanguageBasics::LanguageBasics> :prove add8 === sub8
Q.E.D.
(Total Elapsed Time: 0.014s, using Z3)
```

There is also an `:exhaust` command for finite domains. On occasion
the machinery behind `:prove` gets overwhelmed and, on small enough
domains, exhausting works in a reasonable time.

```sh
labs::LanguageBasics::LanguageBasics> :exhaust add8 === sub8
Using exhaustive testing.
Passed 16 tests.
Q.E.D.
```

The `:check` command is smart enough to notice small enough domains
and switch to exhaustion automagically:

```sh
labs::LanguageBasics::LanguageBasics> :check add8 === sub8
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

* makes cleaner code
* easier for other tools to consume/reason about

### Provide additional types to aid in debugging

Many of the errors in coding Cryptol will be instances of type
mismatching. If you can't see your problem based on the error message,
try adding more type annotations. This
* makes the interpreter do less work trying alternative possibilites
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
