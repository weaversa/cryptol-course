# Introduction

This lab will provide a mostly comprehensive overview of those
components of the Cryptol language needed to complete this course.

## Prerequisites

Before working through this lab, you'll need
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in this file.

## Skills You'll Learn

By the end of this lab you will have a pretty good understanding of
Cryptol's language constructs. At least good enough that you can come
back here for reference as you work through the labs.

Specifically, you'll also gain experience with
  * Cryptol's module system,
  * commenting,
  * Cryptol's `Bit`, sequence, `Integer`, tuple, and record types,
  * evaluating expressions,
  * writing functions and properties, 
  * functions with curried parameters,
  * type parameters and type constraints,
  * the `:check`, `:prove`, and `:sat` commands, 
  * pattern matching,
  * demoting types variables to value variables,
  * `/\`, `\/`, `==>` -- logical operations for single bits,
  * `~`, `&&`, `||`, `^` -- logical operations for sequences,
  * `==`, `!=` -- structural comparison,
  * `==`, `>=`, `>`, `<=`, `<` -- nonnegative-word comparisons,
  * `+`, `-`, `*`, `/`, `%`, `**` -- word-wise modular arithmetic,
  * `>>`, `<<`, `>>>`, `<<<` -- shifts and rotates,
  * `#` -- concatenation,
  * `@`, `!`-- sequence indexing,
  * `@@`, `!!` -- sequence indexing,
  * `if then else` -- conditional expressions,
  * manipulating sequences using `#`, `take`, `drop`, `split`, `join`,
    `head`, `last`, `tail`, `reverse`, `groupBy`, `map`, `iterate`,
    `scanl`, and `foldl`,
  * the `sum` and `carry` operators,
  * enumerations and sequence comprehensions, and
  * lambda functions.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) Cryptol
document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter
running in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::Language::Basics
Loading module Cryptol
Loading module labs::Overview::Overview
Loading module labs::Language::Basics
```

We start by defining a new module for this lab:

```cryptol
module labs::Language::Basics where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Basic Use of the Cryptol Language

If you've programmed in a variety of languages (not just different
[procedural
languages](https://en.wikipedia.org/wiki/Procedural_programming)
descending from C), you'll find that, for the most part, Cryptol is
just another language with a different vocabulary and a funky type
system. After a little study you'll be able to do most anything
computationally that you could otherwise, especially in the
cryptographic realm. But once you are accustomed to Cryptol you will
find that it is much easier to write correct cryptographic programs
than with conventional languages. That's 'cause it's been tuned for
such! To throw out the buzzwords:
  * Cryptol is a [domain-specific
    language](https://en.wikipedia.org/wiki/Domain-specific_language). Not
    only does it have features to support its application domain, but also it elides
    a lot of junk that makes programming and analyzing the programs
    difficult.
  * Cryptol has been designed with automated reasoning about code
    as a priority, so that we can leverage it for verification. Some
    things are harder to do in Cryptol, but they pay off in code that
    can be proven correct!

In some ways this requires a new mind-set:
  * Write properties about your functions.
  * `:check` them.
  * Try to `:prove` them when your function's definition has settled down.

Enjoy getting addicted to this level of assurance!


## Preliminaries

Many of the concepts in this lab were briefly introduced in the
[Overview](../Overview/Overview.md) lab. This lab goes over many of
those same concepts, but in much more depth. Consider this lab a
resource that you may want to revisit as you work through the course
material. Also consider keeping the [official Cryptol
manual](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf)
close at hand.

For examples in this lab, as they are displayed here, the warning
messages about specifying bit sizes of numbers have been turned
off. This is **not** something you should do when you're new at
Cryptol; it's only done here for teaching purposes.

```Xcryptol-session
labs::Language::Basics> :set warnDefaulting = off
```

Also, some examples have octets as outputs that are easier to see as
characters. To see octets as characters, turn on ASCII mode:

```Xcryptol-session
labs::Language::Basics> :set ascii = on
```

That makes any sequence of octets be displayed as the corresponding
[ASCII](https://en.wikipedia.org/wiki/ASCII) string in double quotes
(`"`) and an octet outside a sequence be displayed as the
corresponding ASCII character in single quotes (`'`). (This is mostly
useful as a pedagogical aid.)

```Xcryptol-session
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

## Modules

This file is a Cryptol module. The first interpreted line of every Cryptol module
must be `module Path::...Path::ModuleName where`. The `Path::...Path` component
is the system path from the root of whatever set of modules you're
creating or working from. The `ModuleName` component is the basename
of this file. For instance, this module is `labs::Language::Basics`
because its path from the root repository is
`labs/Language/Basics.md`. There's really not much to naming
modules. But don't forget the `where` clause at the end.

Importing modules is also pretty simple. Just add a line starting with
`import` followed by the name of the module. For example, here we
import the [Overview lab](../Overview/Overview.md).

```comment
import labs::Overview::Overview
```

To avoid naming conflicts, or just generally improve readability, you
can qualify the module import using the `as` clause.

```cryptol
import labs::Overview::Overview as OVLab
```

When the Cryptol interpreter loads the current lab (Basics), it gains access to all
public definitions in the Overview lab.

To keep a definition private, meaning it won't be imported by other
modules, use the `private` clause.

```cryptol
private thisIsPrivate = 10
```

Now all of the Overview lab definitions are accessed by prefixing
`OVLab::`. For example,

```Xcryptol-session
labs::Language::Basics> :browse
...
  From labs::Overview::Overview
  -----------------------------

    OVLab::decrypt : {a} (fin a) => [8] -> [a][8] -> [a][8]
    OVLab::encrypt : {a} (fin a) => [8] -> [a][8] -> [a][8]
    OVLab::sayHello : {a} (fin a) => [a][8] -> [7 + a][8]
...

labs::Language::Basics> :set ascii=on
labs::Language::Basics> OVLab::sayHello "Victoria"
"Hello, Victoria"
```

Imports can be further refined with _import lists_, which specify 
which definitions to include (or exclude) from the imported modules:

```comment
// imports `product` and `distinct` from the NQueens demo
import labs::Demos::Cryptol::NQueens (product, distinct) 

// imports all _except_ the listed test definitions from the CRC spec
import labs::CRC::CRC hiding (
  CRCSimpleTest, testM, CRCSimple_QTest, CRCSimple_XFERTest, 
  CRC32_BZIP2Test, CRC32_CTest, CRC32_DTest, CRC32_MPEG2Test, 
  CRC32_POSIXTest, CRC32_QTest, CRC32_JAMCRCTest, CRC32_XFERTest
)

// imports `littlendian`(`'`) functions, prefaced with `Salsa20::`
import labs::Salsa20::Salsa20 as Salsa20 (littleendian, littleendian')

// imports all except `inc` functions from `ProjectEuler` in `PE::`
import labs::ProjectEuler::ProjectEuler as PE hiding (inc, inc1001)
```

Cryptol's module system also supports parameters, but that is covered
in a later lab.

### File-only commands

It's worth noting that there are a very few Cryptol commands
that can only be used in a file, *not* interactively in the interpreter.
The most common of these are `module`, `import`, `private`, and `property`.  

## Comments

  * `//` comments to the to end of a line
  * `/*` ... `*/` comments a block of code

There is also a [docstring](https://en.wikipedia.org/wiki/Docstring)
comment facility (`/**` ... `*/` preceding a definition):

```cryptol
/**
  * A totally made up identifier for pedagogical purposes. It is
  * used elsewhere for demonstration of something or other.
  */
mask = 7 : [32]
```

Now, when issuing `:help mask`, the above comments are displayed along
with other information about `mask`.

```Xcryptol-session
labs::Language::Basics> :help mask

    mask : [32]

A totally made up identifier for pedagogical purposes. It is
used elsewhere for demonstration of something or other.
```

## Variable and Function Naming

Cryptol identifiers (variable and function names) consist of
alphanumeric characters plus `'` (apostrophe, but read "prime") and
`_` (underscore). They must begin with an alphabetic character or an
underscore. The notational convention for `'` is to indicate a related
definition, while underscore is mostly used to separate words or as a
catch-all for characters we'd like to use but are forbidden. [Camel
case](https://en.wikipedia.org/wiki/Camel_case) is often used when
other naming constraints aren't mandated.

```cryptol
myValue  = 15 : [32]
myValue' = myValue && mask  // mask defined elsewhere
```

Feel free to take a quick look at the [Cryptol style
guide](../../cryptol-style.md) we used for creating the material in
this course.

Technically, Cryptol supports
[Unicode](https://en.wikipedia.org/wiki/Unicode), but this course
doesn't make use of that feature.

## Types of Variables

Cryptol's "basic" data type is an _n_-dimensional array (called a
sequence) whose base type is bits.
  * 0-d: `False : Bit` and `True: Bit`
  * 1-d: Think bytes, words, nibbles, etc., i.e., a sequence of bits
    of any length usually thought of as a number. E.g., `0x2a : [8]`,
    `0b101010 : [6]` and `[False, True, False, True, False, True,
    False] : [7]`. These all compare as 42 in type appropriate
    contexts.
  * 2-d: Think sequences of 1-d objects all of the same size. E.g.,
    `[42, 0b010101010101, 0xa5a, 0o5757] : [4][12]`
  * 3-d: Sequences of 2-d objects all of the same size. E.g., `[[0,
    1], [1, 2], [3, 5], [8, 13]] : [4][2][4]`
  * ...

Things to note:
  * The type of the lowest dimension of every sequence given above is
    `Bit`. It's correct to write `0x2a : [8]Bit`, but the `Bit` part
    of a sequence is implicit in Cryptol, otherwise we'd be writing
    the word `Bit` at the end of just about every type. So, `Bit` is
    often left off when writing the types of sequences.
  * There are no privileged bit widths in Cryptol. `[13]` is just as
    good a type as `[8]`, `[16]`, `[32]` or `[64]`.
  * There's `0b...` for binary, `0o...` for octal and `0x...` for
    hexadecimal.
  * Lengths of sequences may be zero. Zero length sequences act as an
    identity for concatenation and are useful in padding.
  * The possible values by type:
    * `[0]` --- `0`
    * `[1]` --- `0` and `1`
    * `[2]` --- `0`, `1`, `2` and `3`
    * ...
    * `[n]` --- `0` through `2^^n - 1`

  * There are 2<sup>_n_</sup> values of type `[n]`. There are
    2<sup>_mn_</sup> values of type `[m][n]`, etc.
  * 1-d sequences of bits are treated as numbers by arithmetic and
    comparison operators. So for instance, `[False, True] == (1 :
    [2])` and `[True, False, True] > 4` both hold.
  * Cryptol distinguishes between different dimensions. In
    particular, `True` and `[True]` are type incompatible.
  * The number of bracket pairs in a type gives its dimension.
  * Cryptol supports **holes** in types via the `_` character. When
    Cryptol encounters a hole, it will try to infer a value. Notice in
    the example below how Cryptol *fills in* the `3` where we left an
    underscore.

```Xcryptol-session
    labs::Language::Basics> [1, 2, 3] : [_][32]
    [0x00000001, 0x00000002, 0x00000003]
    labs::Language::Basics> :type [1, 2, 3] : [_][32]
    ([1, 2, 3] : [_][32]) : [3][32]
```

Other data types include:
  * Arbitrary-precision integers: E.g., `2^^1023 - 347835479 :
    Integer`
  * Heterogeneous tuples: E.g.: `(False, 0b11) : (Bit, [2])` and
    `(True, [1, 0], 7625597484987) : (Bit, [2][1], Integer)`
    * Elements of tuples are accessed by `.0`, `.1`, ...

```Xcryptol-session
    labs::Language::Basics> (False, 0b11).0
    False
```

  * Records with named fields: E.g., `{flag = True, x = 2} : {flag :
    Bit, x : [4]}`
    * Elements of records are accessed by `.` followed by the field
      name.

```Xcryptol-session
    labs::Language::Basics> {flag = True, x = 2}.flag
    True
```

  * Integers modulo _n_: Each type of the form `[n]`, described above, provides a 
    [least residue
    system](https://en.wikipedia.org/wiki/Modular_arithmetic#Residue_systems)
    for [integers modulo
    2<sup>n</sup>](https://en.wikipedia.org/wiki/Modular_arithmetic#Integers_modulo_n).
    Types of the form `Z n` provide a least residue system for any positive _n_. E.g.,
    `4 + 4 : Z 7` evaluates to `1`.

Though Cryptol supports a slew of different data types, most are not
needed to be successful in this course. Specifically, this course
makes heavy use of sequences, with the occasional tuple and `Integer`
thrown in.

**EXERCISE**: The Cryptol interpreter command `:type` (or `:t` for
short) is very useful for helping understand types. Use the `:type` command
in the interpreter to determine the types of the variables defined below.
(*Recall that these variables have been instantiated as a
result of loading this module via `:module`.*)

```
varType0 = False
varType1 = [False]
varType2 = [False, False, True]
varType3 = 0b001
varType4 = [0x1, 2, 3]
varType5 = [ [1, 2, 3 : [8]] , [4, 5, 6], [7, 8, 9] ]
varType6 = [ [1, 2, 3] : [3][8], [4, 5, 6], [7, 8, 9] ]
varType7 = [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ] : [_][_][8]
varType8 = (0b1010, 0xff)
varType9 = [ (10 : [12], [1, 2 : [4], 3], ([0b101, 7], 0xab)),
             (18 : [12], [0, 1 : [4], 2], ([0b100, 3], 0xcd)) ]
```

For this next set, challenge yourself by filling in your guesses ahead
of time, writing your answers inline below. If you fill in the wrong
type, Cryptol will complain when reloading the file in the
interpreter. For example, say you provided the following type to the
previous `varType0` variable:

```comment
varType0 = False : [10]
```

When you reload this file in the interpreter, you will see the
following error:

```comment
  Type mismatch:
    Expected type: [10]
    Inferred type: Bit
```

Here, Cryptol is telling you that it **expected** the value `False` to
be a 10-bit sequence (because that's what we told Cryptol). However,
Cryptol **inferred** that the type of `False` is actually `Bit`.

The types of the variables you viewed above were all *monomorphic*,
meaning there was only a single valid type for each variable. Recall
that numbers can be represented using a lot of different types. For
instance, the number `5` can be an `Integer`, a 32-bit bitvector, or
even a 12039780-bit bitvector (with 12039777 leading `0` bits). So, 
when you ask for the type of `5` in
the interpreter, you'll see:

```Xcryptol-session
labs::Language::Basics> :t 5
5 : {a} (Literal 5 a) => a
```

That letter `a` inside curly braces is a type variable. When you see a
number (or a function) whose type is stated using a type variable (here, `a`), it means
there is some freedom in the type of the value variable (here,
`5`). For the next set of exercises, you'll be asked to type some
variables monomorphically; that is, you shouldn't need any curly
braces or `=>` symbols when you specify the types. That's all stuff
that's covered later in this section.

**EXERCISE**: Fill in *any valid monomorphic* type for each of the
values below. Some will have multiple correct answers. Once done,
`:reload` this file to check that you've gotten them correct.

```
varType10 = 0x1234

varType11 = 10

varType12 = [1, 2, 0x7]

varType13 = (1, 2, 0x7)

varType14 = [ (1, 2), (3, 4 : Integer), (5 : [10], 6) ]

varType15 = [ 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12 : [8], 13, 14, 15 ]

varType16 = ([10, 9, 8], [7, 6, 5, 0b0100])

varType17 = []

varType18 = ()

varType19 = ([((),()), ((),())], [[[]]], ([[]], [[]]))
```

That last one is a really good demonstration of what to avoid when
writing specifications! Honestly, most of those look such a mess that
it's now obligatory to point out that:

> **Specifications are supposed to be easily understood**. 

It's simply
bad form to mix numerical representations, put type parameters in the
middle of sequences, and so on. Please don't take these exercises to
be considered *good* Cryptol. They were crafted to challenge you,
something you should **never** do to someone who wants to use the
specifications you write. Always strive to make elegant
specifications. There is no need to optimize for performance. Also, don't
write a spec "just to get it done" -- making something that loads and
runs isn't good enough. Aim for creating specifications that *look*
like the mathematics you're specifying. < rant over >

## Types of Functions

What would a programming language be without the ability to write
functions? Since Cryptol is a pure functional language, functions are
stateless (side-effect free) definitions that map each (valid) input to an output.

Here is an example of a function called `add` that takes two arguments
`x` and `y` and adds them together.

```comment
add x y = x + y
```

As it stands, this function works with many different types of `x` and
`y`. For instance, it will work with `x` and `y` both `Integers`, both
13-bit bitvectors, and (believe it or not) even with both as tuples of
sequences. Since this function accepts many different types of
arguments, it's called
[polymorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)):
> provi[ding] a single interface to entities of different types.

Oftentimes, cryptographic functions are written to only work with
specified types (such as having a 256-bit key), and we want to capture
that information in our specifications. Hence, Cryptol functions can
be typed, much the same way as typing variables (in the previous
section). To do so, we add a type definition to the function we're
defining. For an example, here we make our `add` function only work on
32-bit bitvectors:

```comment
add : [32] -> [32] -> [32]
add x y = x + y
```

Type definitions start with the name of the function followed by a
colon. Next, one can *optionally* define some type variables and levy
constraints on them (we describe this later). Then the types of the
input variables are given, separated by `->`. Finally the type of the
output is given.

And we can ask for the type of functions using `:type`, just like we
asked for the types of variables.

```Xcryptol-session
labs::Language::Basics> :type add
add : [32] -> [32] -> [32]
```

### Curried and Uncurried Style

Cryptol functions are often written in the
[curried](https://en.wikipedia.org/wiki/Currying) style:

```cryptol
add : [32] -> [32] -> [32]
add x y = x + y
```

rather than:

```cryptol
addUncurried : ([32], [32]) -> [32]
addUncurried (x, y) = x + y
```

These two functions would be applied as shown:

```Xcryptol-session
labs::Language::Basics> add 20 28
48
labs::Language::Basics> addUncurried (20, 28)
48
```

There's also native support in Cryptol for currying and uncurrying.

```Xcryptol-session
labs::Language::Basics> curry addUncurried 20 28
48
labs::Language::Basics> uncurry add (20, 28)
48
```

Hopefull you can see that these two styles are equivalent at some
level. Curried functions are preferred as they afford [partial
application](https://en.wikipedia.org/wiki/Partial_application), but
uncurried can be useful for explicating the correspondence to
functions from other languages or documents.

  * If it helps you, mentally read curried functions like this: input
    argument types are all prior to the last arrow, and the result type
    follows the last arrow. Pictorially: `in -> in -> ... -> in ->
    out`.
  * Partial application lets one form a new function from an old one
    where an argument is fixed.  For instance, `add 1` is a
    function itself!

```cryptol
increment = add 1
```

```Xcryptol-session
labs::Language::Basics> :t increment
increment : [32] -> [32]
labs::Language::Basics> increment 10
11
```

   `add 1` takes a 32-bit bitvector and returns a 32-bit
    bitvector. When it is applied to a 32-bit bitvector, it adds one to
    that bitvector. Other examples to illustrate partial application:
  * `addUncurried` is really a function of one argument. It happens 
  that the argument is a tuple, which makes 
  `addUncurried (28, 20)` look just like a
  two argument function in many languages.

**EXERCISE**: Use the `:type` command in the interpreter to discover
the types of the following functions.

```
funType0 a = a + 7 : [5]

funType1 a b = a + b + 0b0011100

funType2 a b = (a + 0x12, b + 0x1234)

funType3 (a, b) = (a + 0x12, b + 0x1234)

funType4 ((a, b), c) = c + 10 : [32]

funType5 [a, b, c : [10]] = [a, b, c]

funType6 (a : [3][10]) = [a@0, a@1, a@2]

//Fun fact! funType5 and funType6 compute the same function.
//Try, :prove funType5 === funType6

funType7 x = (x, x, [ [[False, True], x], [x, x], [x, x] ])

funType8 = funType2 10

funType9 = False  //Is this a function with no arguments, or a value? Hmmmm...is there a difference? Nope!
```

Now that you have some experience *viewing* function types, you're
about to be asked to write some. Here are a few common mistakes, and
what the error messages look like in the interpreter:

Say we accidentally added a two input type to `funType0`:

```comment
funType0 : [5] -> [5] -> [5]
funType0 a = a + 7 : [5]
```

Upon reloading this file, we would see:

```comment
  Type mismatch:
    Expected type: [5] -> [5]
    Inferred type: [5]
```

Here Cryptol is telling us that Cryptol
**expected** (based on the type definition) the type of input `a` 
to take two 5-bit bitvectors. But,
Cryptol **inferred** (from the value definition) that the function
just takes a single 5-bit bitvector.

Say we accidentally added the wrong type:

```comment
funType0 : [4] -> [12]
funType0 a = a + 7 : [5]
```

Upon reloading this file, we would see:

```comment
  Type mismatch:
    Expected type: 5
    Inferred type: 4

  Type mismatch:
    Expected type: 12
    Inferred type: 5
```

Now we get two error messages. One is complaining about the input
type, the other about the output type.

**EXERCISE**: Just like in the previous section, you're now being
asked to fill in *any valid monomorphic* type for each of the
functions below. Some will have multiple correct answers. Once done,
reload this file to check that you've gotten them correct.

```

funType10 x = x + x : [10]


funType11 a b = [a : Bit, b, b]


funType12 [a, b] = [a : Bit, b, b]


funType13 (a, b) = [a : Bit, b, b]


funType14 a b ([c, d], e) = [ (a     , [b, b, b]),
                              ([d, d], [c, c, c]),
                              (a     , e) ]


funType15 a b = [ a, b, a, b, a, b,
                  a, b, a, b, a, b,
                  a, b, a, b, a, b,
                  a, b, a, b, a, b,
                  a, b, a, b, a, b,
                  a, b, a, b, a, b,
                  a, b, a, b, a, b ]


funType16 a b = [ [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ] ]


funType17 a b = ( [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ],
                  [ a, b, a, b, a, b ] )


funType18 a b = [ ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ) ]


funType19 a b = ( ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ),
                  ( a, b, a, b, a, b ) )  // We're so sorry
```

After that set of exercises, you likely see the conciseness of the
sequence type over the tuple type (it was hammered in pretty hard
there at the end). Lesson: don't use tuples unless you really really
have to. Curry your parameters and group heterogeneous elements
together in sequences. Tuples are only really useful when you want a
function to output multiple values that have different types.

### Pattern Matching

You've no doubt noticed by now that the left-hand side of assignment
statements aren't restricted to just single variables. This
flexibility comes from Cryptol's powerful **pattern matching**
capabilities. Cryptol allows you to make assignments by writing
patterns based on the type (*shape*) of the value on the right-hand
side. Again, `_` acts as a kind of hole (when it's by itself,
not when it's part of an identifier, of course). For example:

```Xcryptol-session
labs::Language::Basics> let (fst, snd) = (4, 5)
labs::Language::Basics> fst
4
labs::Language::Basics> snd
5
labs::Language::Basics> let r = (0xa, 0xb)
labs::Language::Basics> r
(0xa, 0xb)
labs::Language::Basics> let (fst, snd) = r
labs::Language::Basics> fst
0xa
labs::Language::Basics> snd
0xb
labs::Language::Basics> let [ (a, b, _), (_, _, c), _ ] = [ (1, 2, 3), (4, 5, 6), (7, 8, 9) ] : [3]([4], [4], [4])
labs::Language::Basics> a
0x1
labs::Language::Basics> b
0x2
labs::Language::Basics> c
0x6
```

Cryptol can even pattern match on sequence concatenation; for example:

```cryptol
firstThreeBits : {n} [3 + n]-> [3]
firstThreeBits ([a, b, c] # xs) = [a, b, c]
```

```Xcryptol-session
labs::Language::Basics> :s base=2
labs::Language::Basics> firstThreeBits 0b1100111
0b110
```

### Polymorphic Functions

Sometimes, though not often, cryptographic functions are parameterized
on the type of an input. For example, the [Advanced Encryption
Standard
(AES)](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard)
accepts key sizes of 128, 192, and 256 bits. Also, stream ciphers and
hash functions work over arbitrary sized streams of input. So, in
general, the full type of a function looks something like the following. (For the numbered identifiers, we limit ourselves to two examples, but in reality there can be any number.)

```comment
functionName :
    {typeVariable0, typeVariable1}
    (typeConstraint0, typeConstraint1) =>
    inputType0 -> inputType1 -> outputType
```
Here's an English-language breakdown:

|||
|-|-|
| `functionName` | `:`
| The function `functionName` | has a type

||
|-|
| `{` `typeVariable0` `,` `typeVariable1` `}`
| with type variables `typeVariable0` and `typeVariable1`

|||
|-|-|
| `(` `typeConstraint0` `,` `typeConstraint1` `)` | `=>`
| and the constraints `typeConstraint0` and `typeConstraint1` | applied to the type definition

||||||
|-|-|-|-|-|
| `inputType0` | `->` | `inputType1` | `->` | `outputType`
| that takes `inputType0` | and | `inputType1` | and returns | `outputType`

Let's make an example to work with:

```comment
sayHello:
    {n}
    (fin n) =>
    [n][8] -> [7+n][8]
sayHello name = "Hello, " # name
```
And the breakdown:

|||||||||
|-|-|-|-|-|-|-|-|
| `sayHello` | `:` | `{` `n` `}` | `(` `fin n` `)` | `=>` | `[n][8]` | `->` | `[7+n][8]`
| The function `sayHello` | has a type | with type variable `n` | and the constraint that `n` is finite | applied to the type definition | that takes a list of `n` `8`-bit vectors | and returns | a list of `7+n` `8`-bit vectors

This function's name is `sayHello`, it takes in a sequence called
`name` that is `n` octets long and produces a sequence that is `7+n`
octets long, where `n` is finite. The function itself outputs the
concatenation (using the `#` operator) of the string "Hello, " with
 the value of `name`. If we wanted to enforce that the length of `n` was less than
some value, we could add another constraint, like so:

```cryptol
sayHello:
    {n}
    (n <= 12) =>
    [n][8] -> [7+n][8]
sayHello name = "Hello, " # name
```

Now, since `n` is less than or equal to twelve, it's clearly finite,
so the `fin n` constraint is extraneous. We could leave it, Cryptol
won't complain, but it's nice to be as concise as possible when typing
functions. A list of all available type constraints can be found by
typing `:browse` into the interpreter and looking for the "Primitive
Types" section. You can also ask for `:help` on any of these, for
example:

```Xcryptol-session
labs::Language::Basics> :h fin

    primitive type fin : # -> Prop

Assert that a numeric type is a proper natural number (not 'inf').
```

Let's use the interpreter to send a few values through `sayHello` and
see what happens.

```Xcryptol-session
labs::Language::Basics> :s ascii=on
labs::Language::Basics> sayHello "Munkustrap"
"Hello, Munkustrap"
labs::Language::Basics> sayHello "Skimbleshanks"

[error] at <interactive>:1:1--1:25:
  Unsolvable constraints:
    • 12 >= 13
        arising from
        use of expression sayHello
        at <interactive>:1:1--1:9
    • Reason: It is not the case that 12 >= 13
```

Here we see that `sayHello` happily accepts a 10-octet sequence but
wholeheartedly rejects a 13-octet sequence. This is the type system
in action! Let's also briefly take a look at the type for the
concatenation operator `#`.

```Xcryptol-session
labs::Language::Basics> :t (#)
(#) : {front, back, a} (fin front) =>
        [front]a -> [back]a -> [front + back]a
```

We can pretty accurately guess what this function is doing just by
looking at its type. Here we see that `#` takes two arguments. The
first is a sequence of `front` many elements of type `a`. The second
is a sequence of `back` many elements, also of type `a`. The function
returns a sequence of `front + back` many elements, again of type
`a`. The type variable `a` is unconstrained (there are no type
constraints levied on it) and so `#` will accept sequences of any type
--- a sequence of bits, a sequence of sequences, a sequence of tuples,
etc. The `fin front` constraint tells us that the first sequence has
to have a finite number of elements, and the absence of constraints on
`back` means that the second argument can have any number (even an
infinite number) of elements. Since sequences (in some sense) start
from the left, it doesn't make sense to concatenate something onto the
back of an infinite sequence, but it seems perfectly fine to
concatenate something onto the front of an infinite sequence.

All that aside, the `sayHello` example above is a bit silly, but when
utilized fully, the type system acts to protect functions from being
called in a way that is potentially harmful. For example, let's say we
had a function that accesses the 12th bit of a bitvector. This can be
achieved using Cryptol's `@` operator which performs 0-based indexing
from the left (indexing sequences is explained in more detail later
on). The type system should be used to make sure that such a function
can only be called with bitvectors with at least 13 bits, like so:

```cryptol
bitTwelve :
    {n}
    (fin n, n >= 13) =>
    [n] -> Bit
bitTwelve x = x@12
```

Let's use the interpreter to send a few values through `bitTwelve` and
see what happens.

```Xcryptol-session
labs::Language::Basics> bitTwelve 0b101100101001

[error] at <interactive>:1:1--1:25:
  Unsolvable constraints:
    • 12 >= 13
        arising from
        use of expression bitTwelve
        at <interactive>:1:1--1:10
    • Reason: It is not the case that 12 >= 13
labs::Language::Basics> bitTwelve 0b1010101010100100101010101010101010101
False
```

As expected, the type constraint forces the function to only accept
bitvectors with more than 12 bits.

For this course, we restrict ourselves to type variables and
constraints involved with sequences. This means (while you work
through the material here) you can always think about type variables
as representing the **sizes** (or length) of sequences, and type
constraints as constraints on those sizes. Type variables and
constraints can represent more, but these extensions are not used or
covered in this course.

With this idea in mind (type variables as sizes), many procedural
programming languages treat the sizes of sequences as value variables
. For example, in C, one needs to pass the length of an array as a
value variable, like so:

```C
int F (int *array, int size)
```

And `F` has to trust that the length of `array` really is
`size`. Whereas in Cryptol we would write `F` as:

```comment
F : {size} (fin size) => [size][32] -> [32]
```

In Cryptol, `array` and `size` are different classes of variables, and they are 
strongly linked so that `F` doesn't have to trust that the length of
`array` really is `size`. This kind of linkage is called [Strong
typing](https://en.wikipedia.org/wiki/Strong_and_weak_typing) and generally 
refers to use of programming language types in order to both
capture invariants of the code, and ensure its correctness, and
definitely exclude certain classes of programming errors.

Now, because there are two classes of variables, type variables and value 
variables, there are distinct ways of passing them to a function. The
material above demonstrated passing value variables. We'll now
demonstrate how to pass type variables using the backtick `` ` ``
character (usually on a key shared with `~` positioned in the upper
left of the keyboard) and curly braces `{}`.

Let's make a function that repeats a value of type `a` exactly `n`
times, where `a` and `n` are type variables. To create a repeating sequence,
this function uses what's called a **sequence comprehension**, but you
can ignore that for now; it gets covered later.

(To clarify:  The value of the type variable `n` will be a number.  The 
value of the type variable `a` will be a type, such as Bit or [16].  This 
value of `a` will be the type of the value that will get repeated. That 
value to be repeated is input as the argument of the `repeat` function.) 

```cryptol
repeat :
    {n, a}
    () =>
    a -> [n]a
repeat value = [ value | _ <- zero : [n] ]
```

**EXERCISE**: Here are a few examples demonstrating how to pass type
and value variables to this function. Please try typing these examples
into the interpreter and consider the output, and trying your own
examples as curiosity strikes you.

To clarify some of the terminology used here: In Cryptol, when we speak
of “passing a variable,” it’s basically a short way to say “passing the 
value of (that) variable.” This applies both to passing a type variable/
passing the value of a type variable, and to passing a value variable/
passing the value of a value variable.  (The shorter form is in a sense 
more abstract, though.)

For instance, in the first example below (`polyType0`), we pass two type 
variables to the `repeat` function as parameters, namely, `a` and `n`.  
But specifically, we pass their values `[64]` and `2`, respectively.  We 
also pass to the `repeat` function the value `7` as its argument.

```cryptol
polyType0 = repeat`{a=[64], n=2} 7

polyType1 = repeat`{n=4, a=[64]} 7

polyType2 = repeat`{a=Bit, n=20} True

polyType3 = repeat`{n=20, a=Bit} True

polyType4 = repeat`{n=20} True

polyType5 = repeat`{n=4, a=[2][3]} zero

polyType6 = repeat`{n=4, a=[3][7]} [1, 2, 3]

polyType7 = repeat`{n=4} ([1, 2, 3] : [3][7])

polyType8 = repeat 7 : [5][16]

polyType9 = repeat`{a=[16]} 7 : [5][_]

polyType10 = repeat`{n=5} 7 : [_][16]

polyType11 = repeat`{a=[16], n=5} 7

polyType12 = repeat`{5, [16]} 7

polyType13 = repeat`{5} (7 : [16])
```

You'll notice that you can either pass type variable values or let Cryptol infer
the type variable values from the type of the output. Also, those last two examples
demonstrate that you can pass type parameters based on position,
that is, since the type of repeat declares `{n, a}` as type variables
**in that order** (`n` first, then `a` second), Cryptol will infer
which value goes with which type variable based on its position inside
the curly braces, so you don't need to provide the `name=` part.

**EXERCISE**: Write a function called `zeroPrepend` that prepends `n`
`False` bits onto the beginning of an `m`-bit bitvector called
`input`. You'll need to use the `#` operator. Feel free to use the
`repeat` function we wrote above, though there are solutions that
don't require it.

(Note:  Many times in this course you will be asked to do a coding exercise in
which your assignment is to alter a snippet of code.  If when doing so you
find you need to start over, but you have saved over the original code snippet
and do not know what the original looked like, you may find the original by
locating the current module in the course repository on
[GitHub](https://github.com/weaversa/cryptol-course).)

```cryptol
// Uncomment and fill in
//zeroPrepend : {} () => _ -> _
zeroPrepend input = undefined
```

Check your function by running these tests in the interpreter and
seeing that you get the same results:

```Xcryptol-session
labs::Language::Basics> :s base=2
labs::Language::Basics> zeroPrepend`{n=7} 0b111
0b0000000111
labs::Language::Basics> zeroPrepend`{n=3, m=inf} zero
[False, False, False, False, False, ...]
labs::Language::Basics> zeroPrepend`{m=6} 5 : [10]
0b0000000101
labs::Language::Basics> zeroPrepend`{5, 6} 15
0b00000001111
labs::Language::Basics> :s base=16
```

### Demoting Types to Values

Because type variables and value variables are different classes of
variables, they cannot interact directly (for example, we cannot write
an expression equating the two). If we think of these two
classes being in a hierarchy, type variables would be above value
variables. With this hierarchy in mind, Cryptol does allow type variables
to be **demoted** to value variables, but value variables cannot be
promoted to type variables. For example, the following is not
possible.

```comment
notPossible size = 0 : [size]
```

We cannot go from a value variable (`size` on the left) up to a type
variable (`size` on the right). However, we can go down by using the backtick `` ` ``
character. For example:

```cryptol
appendSize :
    {size}
    (fin size, 32 >= width size) =>
    [size][32] -> [size+1][32]
appendSize input = input # [`size]
```

Here we concatenate the size of a sequence onto the end of that sequence. To read the
function definition more verbatim: 
>`appendSize` takes an input named
`input` that is a sequence of `size` number of 32-bit elements and
outputs a sequence of `size+1` 32-bit elements, where the first `size`
elements are `input` and the last element is the size of the input
sequence. 

When type variables are demoted to value variables, they
must take on a type. Cryptol usually infers the correct type, and in
this case `` `size `` becomes a 32-bit value. It is because of this
that the function has `32 >= width size` as a type constraint. If
`size` were greater than `2^^32`, it couldn't be demoted into a 32-bit
value because it wouldn't fit! So, the demotion here forces us to add
this extra type constraint. Luckily, if you forget to add such things,
Cryptol will generally complain and let you know what you forgot. For
example, if we remove that constraint and reload this file we see:

```Xcryptol-session
  Failed to validate user-specified signature.
    in the definition of 'appendSize', at Basics.md:923:1--923:11,
    we need to show that
      for any type size
      assuming
        • fin size
      the following constraints hold:
        • 32 >= width size
            arising from
            use of literal or demoted expression
```

This essentially says that Cryptol won't accept the function unless we
add the constraint that `32 >= width size`, or some stronger
constraint --- we could, if we wanted, add that `size < 7` as it
subsumes the more general constraint that Cryptol is inferring.

As a quick aside, you may be wondering why `` `size `` is inside
brackets (`[]`). This is due to the fact that Cryptol can only
concatenate sequences that are the same dimension. `input` is a
2-dimensional sequence, and `` `size `` is a 1-dimensional
sequence. So we surround it in brackets (`` [`size] ``) to turn it
from a 32-bit bitvector into a sequence of one 32-bit bitvector (a
2-dimensional sequence).

### Type synonyms

You can define type synonyms using the `type` keyword. For example

```cryptol
type myType x = [x][x]
```

```Xcryptol-session
labs::Language::Basics> :s base=2
labs::Language::Basics> zero : myType 2
[0b00, 0b00]
labs::Language::Basics> zero : myType 5
[0b00000, 0b00000, 0b00000, 0b00000, 0b00000]
```

## Judicious Type System Usage

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
signatures.  Impacts:

  * cleaner code
  * easier for other tools to consume/reason about

### Provide additional types to aid in debugging

Many of the errors in coding Cryptol will be instances of type
mismatching. If you can't see your problem based on the error message,
try adding more type annotations. This:
  * makes the interpreter do less work trying alternative
    possibilities and, consequently, can make error messages more
    comprehensible
  * reduces the body of code to examine for bugs (a sort of binary bug
    search)
  * can get you to notice where you mucked up

## Local Definitions

All the functions we've written so far have been one-liners (well,
essentially). This section introduces the `where` clause, a mechanism
that allows you to create local definitions in functions. There's
really not too much to this, but you'll use it in almost every Cryptol
function you'll ever write, so consider it important.

Here we describe what a function looks like in Cryptol. (For the numbered identifiers here, we limit ourselves to two or three examples, but in reality there can be any number.)

```comment
functionName :
    {typeVariable0, typeVariable1}
    (typeConstraint0, typeConstraint1) =>
    inputType0 -> inputType1 -> outputType
functionName input0 input1 =
    output
  where
    localVariable0 = expression0
    localVariable1 = expression1
    output = expression2
```
Here's a breakdown of how to read it:

Function type specification:

|||
|-|-|
| `functionName` | `:`
| The function `functionName` | has a type

||
|-|
| `{` `typeVariable0` `,` `typeVariable1` `}`
| with type variables `typeVariable0` and `typeVariable1`

|||
|-|-|
| `(` `typeConstraint0` `,` `typeConstraint1` `)` | `=>`
| and the constraints `typeConstraint0` and `typeConstraint1` | applied to the type definition

||||||
|-|-|-|-|-|
| `inputType0` | `->` | `inputType1` | `->` | `outputType`
| that takes `inputType0` | and | `inputType1` | and returns | `outputType`.

Function definition:

|||||
|-|-|-|-|
| `functionName` | `input0` | `input1` | `=`
| The function `functionName` | takes `input0` | and `input1` | and returns

||
|-|
| `output` |
| the value of `output`,

||
|-|
| `where` |
| which is computed after 

||
|-|
| `localVariable0` `=` `expression0`
| `localVariable0` is assigned the value of `expression0`,

||
|-|
| `localVariable1` `=` `expression1`
| `localVariable1` is assigned the value of `expression1`,

||
|-|
| `output` `=` `expression2`
| and `output` is assigned the value of `expression0`.

Here's an example that demonstrates the use of a `where` clause:

```cryptol
addMult :
    {n}
    (fin n) =>
    [n] -> [n] -> [n] -> [n]
addMult a b c = ab + bc
  where
    ab = a * b
    bc = b * c
```
And the breakdown:

|||||||||||||
|-|-|-|-|-|-|-|-|-|-|-|-|
| `addMult` | `:` | `{n}` | `(fin n)` | `=>` | `[n]` | `->` | `[n]` | `->` | `[n]` | `->` | `[n]`
| The function `addmult` | has a type | with type variable `n` | and the constraint that `n` is finite | applied to the type definition | that takes an `n`-bit vector | and&nbsp;  | an `n`-bit vector | and&nbsp; | an `n`-bit vector | and returns | an `n`-bit vector.

## Properties

Cryptol has a built-in automated theorem proving interface. This lab
doesn't go over this capability except to say that you can designate
functions with an output type of `Bit` as properties using the
`property` keyword. The purpose of this keyword is mostly to help
document and differentiate functions that are used to compute a
cryptographic algorithm, with functions that express properties about
a cryptographic algorithm. For example, here we have two ways to
compute the same function, and a property stating that they are
equivalent for all inputs:

```cryptol
/**
 * Checks if any of the 4 bytes of a 32 bit word are zero. Returns True
 * if any byte is zero, returns False otherwise.
 */
anyZeroByteOpt : [32] -> Bit
anyZeroByteOpt v =
  ~((((v && 0x7F7F7F7F) + 0x7F7F7F7F) || v) || 0x7F7F7F7F) != 0
  
anyZeroByteSpec : [32] -> Bit
anyZeroByteSpec bytes =
    b0 == 0 \/ b1 == 0 \/ b2 == 0 \/ b3 == 0
  where
    [b0, b1, b2, b3] = split bytes : [4][8]
    
property anyZeroByteCorrect bytes =
    anyZeroByteOpt bytes == anyZeroByteSpec bytes
```

Cryptol's `:prove` interpreter command will cue on the `property`
keyword, trying to prove every `property` in scope. The `:prove`
command also works if you give it a property directly, like so:

```Xcryptol-session
labs::Language::Basics> :prove anyZeroByteCorrect
Q.E.D.
(Total Elapsed Time: 0.009s, using "Z3")
```

Here, `:prove` tells Cryptol to call out to an external theorem prover
(here, Z3) to try and prove the property. Cryptol also supports a
light-weight quick check interface `:check` that runs some random
inputs through a property, rather than trying to prove it for all
inputs. Cryptol also allows you to *find* solutions to a property via
its `:sat` command. For example,

```Xcryptol-session
labs::Language::Basics> :sat \x -> increment x < x
Satisfiable
(\x -> increment x < x) 0xffffffff = True
(Total Elapsed Time: 0.009s, using "Z3")
```

Here we used a *lambda* function (indicated by `\`), 
a simple way to create a function
without giving it a name. We'd read the above as, "Cryptol, find an
assignment to `x` such that `increment x < x`." And since the type of
`increment` forces `x` to be a 32-bit bitvector, `increment
0xffffffff` overflows to zero, yielding the solution 0xffffffff.

## Operators

Cryptol's `:help` command will provide a brief description of an
operator by issuing `:help` (`:h` for short)
followed by the name of the operator in parentheses. For example:

```Xcryptol-session
Cryptol> :help (@)

    (@) : {n, a, ix} (fin ix) => [n]a -> [ix] -> a

Precedence 100, associates to the left.

Index operator.  The first argument is a sequence.  The second argument is
the zero-based index of the element to select from the sequence.
```

Many languages differentiate signed and unsigned numbers at the type
level (e.g. C's `uint32` and `int32`). Cryptol has separate operators
for signed operations which are indicated by a suffixed `$`. Most of
the time you don't need them, as cryptography tends to use nonnegative
numbers. In case you do, Cryptol also has `carry`, `scarry`, and
`sborrow` operators for computing overflow and underflow of addition
and subtraction.

Where appropriate, operators act element-wise (or "blast through")
typing constructs like sequences, tuples and records.

```Xcryptol-session
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
and show some tricks of Cryptol. Feel free to follow along by running
these examples in the interpreter yourself.

### Arithmetic: `+`, `-`, `*`, `/`, `%` and `^^`
#### Signed versions: `/$` and `%$`

```Xcryptol-session
labs::Language::Basics> 1 + 1
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of '(Cryptol::+)'
2
labs::Language::Basics> 1 + 1 : [1]
0x0
labs::Language::Basics> 2^^127 - 1 // a 33 digit Mersenne prime
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of '(Cryptol::^^)'
170141183460469231731687303715884105727
```

The first example defaults to type `Integer`. In the second, 1-bit
addition is explicitly stated so that the computation is essentially
modular addition (XOR). The third shows that `^^` is exponentiation.

The division (`/`) operation is not what a mathematician imagines in
modular arithmetic. For instance `3 * 3 == 1 : [3]` so a mathematician
would expect `1 / 3 == 3 : [3]` since division is the inverse of
multiplication. However, in Cryptol `1 / 3 == 0 :
[3]`. Mathematically, `/` and `%` yield the quotient and remainder,
respectively, in
[Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division).

### Bitwise logical: negation `~`, conjunction `&&`, disjunction `||` and exclusive-or `^`

```Xcryptol-session
labs::Language::Basics> :s base=2
labs::Language::Basics> ~0b000011001101
0b111100110010
labs::Language::Basics> 0b111100110010 && 0b100001001101
0b100000000000
labs::Language::Basics> 0b000011010000 ^ 0b000000001001
0b000011011001
labs::Language::Basics> 0b100000000000 || 0b000011011001
0b100011011001
```

### Comparison:`==`, `!=`, `<` , `<=`, `>` and `>=`
#### Signed versions: `<$`, `<=$`, `>$` and `>=$`

```Xcryptol-session
labs::Language::Basics> [~1, 1] == [6 : [3], 3 * 3]
True
labs::Language::Basics> [~1, 1] == [6 : [4], 3 * 3]
False
```

In the first example, `6` is the literal value that requires the most bits and is
given type `[3]`. That makes both sides of the equality test have type
`[2][3]` (two elements of three bits each). Now `~1 : [3]` is `0b110`
or `6` in decimal and `3 * 3 : [3]` is `1 : [3]`, so `[~1, 1] == [6, 1]
== [6, 3 * 3]`.

In the second example, `6` is given type `[4]`, so both sides have
type `[2][4]`. Now `~1 : [4]` is `0b1110` or `14` in decimal and
`3 * 3 : [4]` is `9` in decimal. We have `[~1, 1] == [14, 1]`
while `[6, 3 * 3] == [6, 9]`, so equality fails.

_**It can be crucially important to be precise about the widths of
things!**_

```Xcryptol-session
labs::Language::Basics> (1:[3]) <$ 2
True
labs::Language::Basics> (1:[3]) <$ -2
False
labs::Language::Basics> (1:[3]) < -2
True
labs::Language::Basics> 1 < -2
False
labs::Language::Basics> 1 <$ -2

Cannot evaluate polymorphic value.
Type: (Error SignedCmp Integer) => Bit
```

Comparisons are lexicographic on sequences of numbers.

```Xcryptol-session
labs::Language::Basics> [1, 2] < [1, 3]
True
labs::Language::Basics> [1, 2] < [1, 2]
False
```

### Shifts and Rotates: `<<`, `>>`, `<<<` and `>>>`
#### Signed version: `>>$`

```Xcryptol-session
labs::Language::Basics> :set base=16
labs::Language::Basics> 0xa5a << 4
0x5a0
labs::Language::Basics> 0xa5a << 12
0x000
labs::Language::Basics> 0xa5a <<< 16
0x5aa
```

### Indexing and slicing: `@`, `!`, `@@` and `!!`

```Xcryptol-session
labs::Language::Basics> :set ascii=on
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

```Xcryptol-session
labs::Language::Basics> "dog" # "cow" // Moof!
"dogcow"
```

### Shortcutting logical: `/\`, `\/` and `==>`

These are most often used in property statements. `/\` is "and", `\/`
is "or" and `==>` is "implies". They have very low precedence.

```Xcryptol-session
labs::Language::Basics> 1 == 5 \/ 5 == 5
True
labs::Language::Basics> False ==> 1 == 5 /\ 1 != 5
True
```

### `if ... then ... else`

Cryptol's `if ... then ... else` is much like C's ternary operator
`?`...`:`. It is not like the `if ... then ... else` control
structure.

```Xcryptol-session
labs::Language::Basics> 2 + (if 10 < 7 then 12 else 4) + 2 : Integer
8
```

**EXERCISE**: Specify the Speck2n round function from page 14 of the
[Simon and Speck specification document](404.pdf). Here we provide
you with the S (rotate) functions and the inverse round function
`R'`. There are also two properties that you can use to prove your
work. An example of how to do this follows below.

```cryptol
S amount value = value <<< amount

S' amount value = value >>> amount

//Uncomment and fill in this definition according to the Speck specification:
//R :
//  {?}
//  (?) =>
//  ?
R k (x, y) =
    undefined

R' :
    {n}
    (fin n) =>
    [n] -> ([n], [n]) -> ([n], [n])
R' k (x, y) =
    (S a ((x ^ k) - S' b (x ^ y)), S' b (x ^ y))
  where
    n = `n : Integer
    a = if n == 16 then 7 else 8 : [4]
    b = if n == 16 then 2 else 3 : [2]

RInverseProperty :
    {n}
    (fin n) =>
    [n] -> ([n], [n]) -> Bit
property RInverseProperty k (x, y) =
    R' k (R k (x, y)) == (x, y)
```

Here we demonstrate proving `RInverseProperty` for 32-bit inputs and
64-bit inputs. You should run the following commands after writing
your specification of `R`. Cryptol will tell you when your `R` is
correct by printing `Q.E.D.`. This means Cryptol has proven that your
`R` is correct for all possible inputs (which is either `2^^96` for
the 32-bit proof or `2^^192` for the 64-bit proof).

```Xcryptol-session
labs::Language::Basics> :prove RInverseProperty`{32}
Q.E.D.
(Total Elapsed Time: 0.008s, using "Z3")
labs::Language::Basics> :prove RInverseProperty`{64}
Q.E.D.
(Total Elapsed Time: 0.008s, using "Z3")
```

## Common Primitives

Again, Cryptol's `:help` command will provide a brief description of the
primitives in the section by issuing `:help` followed
by the name of the primitive.

### Collections of all `False` or all `True` bits

  * `0` is a sequence of `False` bits whose type is determined by the
    context.

```Xcryptol-session
  labs::Language::Basics> 0 : [12]
  0x000
```

  * `zero` is an arbitrary collection of `False` bits whose type is
    determined by the context.

```Xcryptol-session
  labs::Language::Basics> zero : ([8], [4])
  (0x00, 0x0)
```

Here we have produced an ordered pair of a 0 octet and a 0 nibble.
  * `~0` and `~zero` produce all `True` bits correspondingly.


### List manipulation: `take`, `drop`, `head`, `tail`, `last` and `reverse`

```Xcryptol-session
labs::Language::Basics> take "dogcow" : [3][8]
"dog"
labs::Language::Basics> drop [2, 3, 5, 7, 11] : [3]Integer
[5, 7, 11]
labs::Language::Basics> head [1, 2, 3] : Integer
1
labs::Language::Basics> tail [0, 1, 1] : [2]Integer
[1, 1]
labs::Language::Basics> last [2, 3, 5, 7, 11] : Integer
11
labs::Language::Basics> reverse [0, 0, 1] : [3]Integer
[1, 0, 0]
```

Of course, the sizes of lists have to be big enough for 
the requested operation. Also, notice that
`head` (which is equivalent to `@0`) and `last` (which is equivalent to
`!0`) return an element, while the others return lists.

Often in a Cryptol program, the context will determine the shapes of
sequences, so that the type annotations (`: [3][8]` and `: [3]Integer`
above) will then be unnecessary.

### List shape manipulation: `split`, `groupBy`, `join`, `transpose`

```Xcryptol-session
labs::Language::Basics> split`{8} 0xdeadbeef
[0xd, 0xe, 0xa, 0xd, 0xb, 0xe, 0xe, 0xf]
labs::Language::Basics> groupBy`{4} 0xdeadbeef
[0xd, 0xe, 0xa, 0xd, 0xb, 0xe, 0xe, 0xf]
labs::Language::Basics> join [0xca, 0xfe]
0xcafe
labs::Language::Basics> transpose [[1, 2], [3, 4]]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
[[1, 3], [2, 4]]
```

### Functional programming operators: `sum`, `map`, `iterate`, `scanl`, `foldl`

Cryptol supports a few common idioms in functional programming. This
section briefly touches upon five of these.

The `sum` operator takes a sequence of elements and accumulates them.
Similar to other operators, `sum` acts element-wise, and as such
accepts sequences of any type that arithmetic can be applied to.

```Xcryptol-session
labs::Language::Basics> sum [1, 2, 3, 4, 5]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
15
labs::Language::Basics> sum [ [1, 2], [3, 4], [5, 6] ]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
[9, 12]
labs::Language::Basics> sum (sum [ [1, 2], [3, 4], [5, 6] ])
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
21
```

The `map` operator applies an operation to each element in a sequence.

```Xcryptol-session
labs::Language::Basics> :s base=10
labs::Language::Basics> map increment [1, 2, 3, 4, 5]
[2, 3, 4, 5, 6]
labs::Language::Basics> let sumt (a, b) = a + b
labs::Language::Basics> map sumt [ (1, 2), (3, 4), (4, 5) ]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of 1st tuple field
[3, 7, 9]
labs::Language::Basics> :s base=2
labs::Language::Basics> map reverse [0b10110, 0b00101, 0b00111]
[0b01101, 0b10100, 0b11100]
```

The `iterate` operator maps a function iteratively over an initial
value, producing an infinite list of successive function applications.

```Xcryptol-session
labs::Language::Basics> :s base=10
labs::Language::Basics> iterate increment 0
[0, 1, 2, 3, 4, ...]
labs::Language::Basics> let skipBy a x = x + a
labs::Language::Basics> iterate (skipBy 3) 0
Showing a specific instance of polymorphic result:
  * Using 'Integer' for 1st type argument of '<interactive>::skipBy'
[0, 3, 6, 9, 12, ...]
```

The `scanl` operator transitions an initial state given a 'next state'
function and a sequence of elements to act on at each transition.  `scanl` 
returns the sequence of initial and transitioned states. `foldl`, which 
you may find more useful, works just like `scanl`, but returns only the final 
state after all transitions. In fact, `foldl == last scanl`.

```Xcryptol-session
labs::Language::Basics> scanl (+) 0 [1, 2, 3, 4, 5]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
[0, 1, 3, 6, 10, 15]
labs::Language::Basics> foldl (+) 0 [1, 2, 3, 4, 5]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
15
labs::Language::Basics> let step state c = if c == True then state+1 else state-1
labs::Language::Basics> scanl step 0 [True, True, False, False, True]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for 1st type argument of '<interactive>::step'
[0, 1, 2, 1, 0, 1]
labs::Language::Basics> foldl step 0 [True, True, False, False, True]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for 1st type argument of '<interactive>::step'
1
labs::Language::Basics> :s base=2
labs::Language::Basics> scanl (<<) 1 [1, 2, 3, 4] : [5][12]
[0b000000000001, 0b000000000010, 0b000000001000, 0b000001000000, 0b010000000000]
labs::Language::Basics> foldl (<<) 1 [1, 2, 3, 4] : [12]
0b010000000000
```

In most Cryptol programs, the context will enforce the size of things,
so the type annotations shown in these examples need not be present.

## Small Functions

Cryptol programs are just sequences of appropriate functions applied
in the correct order. Good Cryptol features small, easy to understand
functions composed into conceptually bigger ones. This is good
computer science in general, but in Cryptol it is even more
advantageous:
  * Easy to test --- Cryptol's interpreter makes it very cheap to try
    your functions out.
  * Encourages programming with properties --- Properties can be
    tested easily and, as we'll see, proven to provide guarantees
    about code. Moreover, properties serve as another kind of
    documentation!

## Writing Loops

### Or not...

Many of Cryptol's operators naturally extend element-wise over
    nested sequences to any depth.

```Xcryptol-session
labs::Language::Basics> [[[2, 3], [5, 7]], [[11, 13], [17, 19]]] + [[[0, 1], [1, 2]], [[3, 5], [8, 13]]]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type of sequence member
[[[2, 4], [6, 9]], [[14, 18], [25, 32]]]
```

So we don't have to write loops within loops to process these
    sorts of multidimensional arrays.
All of the arithmetic, bitwise logical, and comparison operators work
    element-wise over nested sequences!

### Loop indices

Enumerations serve to provide the indices to loops.

```Xcryptol-session
labs::Language::Basics> [1..10]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
labs::Language::Basics> [1, 3..10]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromThenTo'
[1, 3, 5, 7, 9]
```

#### Infinite indices

You can have "infinite" enumerations with `...`.

```Xcryptol-session
labs::Language::Basics> [1...]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::infFrom'
[1, 2, 3, 4, 5, ...]
```

So long as only a finite prefix of any "infinite" calculation is
needed we're fine.

### Loops to accumulate a value

Loops to accumulate a value are often simple calculations over
indices.

```Xcryptol-session
labs::Language::Basics> sum [1..100]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
5050
```

### Loops with functions on the indices

Loops with functions on the indices are written as **sequence
comprehensions**. From the [Cryptol
manual](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf),
Section 1.6.2:

> A Cryptol comprehension is a way of programmatically computing the
> elements of a new sequence, out of the elements of existing
> ones. The syntax is reminiscent of the set comprehension notation
> from ordinary mathematics, generalized to cover parallel branches

```Xcryptol-session
labs::Language::Basics> [ n^^3 | n <- [0..10] ]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
[0, 1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]
```

Star Trek's (T.O.S.) warp factor light speed multipliers!

||||||||
|-|-|-|-|-|-|-|
| `[` | `n^^3` | `\|` | `n` | `<-` | `[0..10]` | `]`
| Generate the sequence | with elements of the form `n^^3` | where | `n` | draws from | the sequence `0` through `10`
||||||||

We refer to the right-hand side (`n <- [0..10]`)
as a branch. With multiple branches, there are two choices for how the
values are drawn from the branches, *cartesian* (`,` between
branches), or in *parallel* (`|` between branches). For example:

```Xcryptol-session
labs::Language::Basics> [ (a, b) | a <- [0..3] , b <- [0..7] ]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
[(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7),
 (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7),
 (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7),
 (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7)]
labs::Language::Basics> [ (a, b) | a <- [0..3] | b <- [0..7] ]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
[(0, 0), (1, 1), (2, 2), (3, 3)]
```

These two types can mix, though this is not often found when
specifying cryptography.

```Xcryptol-session
labs::Language::Basics> [ (a, b, c) | a <- [0..2] , b <- [3..4] | c <- [5..10] ]
Showing a specific instance of polymorphic result:
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
  * Using 'Integer' for type argument 'a' of 'Cryptol::fromTo'
[(0, 3, 5), (0, 4, 6), (1, 3, 7), (1, 4, 8), (2, 3, 9), (2, 4, 10)]
```

The previously described functional programming idioms can all be
implemented using sequence comprehension. For example:

  * `map`
    ```Xcryptol-session
    labs::Language::Basics> let sumt (a, b) = a + b
    labs::Language::Basics> map sumt [ (1, 2), (3, 4), (4, 5) ]
    Showing a specific instance of polymorphic result:
      * Using 'Integer' for type of 1st tuple field
    [3, 7, 9]
    labs::Language::Basics> [ sumt (a, b) | (a, b) <- [ (1, 2), (3, 4), (4, 5) ] ]
    Showing a specific instance of polymorphic result:
      * Using 'Integer' for the type of '<interactive>::b'
    [3, 7, 9]
    ```
  * `iterate`
    ```Xcryptol-session
    labs::Language::Basics> let skipBy a x = x + a : Integer
    labs::Language::Basics> iterate (skipBy 3) 0
    [0, 3, 6, 9, 12, ...]
    labs::Language::Basics> let seq = [0] # [ skipBy 3 s | s <- seq ]
    labs::Language::Basics> seq
    [0, 3, 6, 9, 12, ...]
    ```
  * `scanl` and `foldl`
    ```Xcryptol-session
    labs::Language::Basics> scanl (+) 0 [1, 2, 3, 4, 5]
    Showing a specific instance of polymorphic result:
      * Using 'Integer' for type of sequence member
    [0, 1, 3, 6, 10, 15]
    labs::Language::Basics> let seq = [0] # [ a + b | a <- seq | b <- [1, 2, 3, 4, 5] ]
    labs::Language::Basics> seq
    Showing a specific instance of polymorphic result:
      * Using 'Integer' for the type of 'b'
    [0, 1, 3, 6, 10, 15]
    labs::Language::Basics> foldl (+) 0 [1, 2, 3, 4, 5]
    Showing a specific instance of polymorphic result:
      * Using 'Integer' for type of sequence member
    15
    labs::Language::Basics> last seq
    Showing a specific instance of polymorphic result:
      * Using 'Integer' for 1st type argument of '<interactive>::seq'
    15
    ```

### Loops that modify an accumulator in place

Loops that modify an accumulator in place become self-referential
sequence comprehensions. The following example illustrates this.

## Simple Block Encryption Example

```cryptol
keyExpand : [32] -> [10][32]
keyExpand key = take roundKeys // take leverages the type signature
  where
    roundKeys : [inf][32]  // a conceptually infinite list
    roundKeys = [key] # [ roundKey <<< 1 | roundKey <- roundKeys ]

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

Here's an English-language breakdown of the first self-referential
sequence comprehension above:

||||||||||||
|-|-|-|-|-|-|-|-|-|-|-|
| `roundKeys` | `=` | `[key]` | `#` | `[` | `roundKey <<< 1` | `\|` | `roundKey` | `<-` | `roundKeys` | `]`
| The sequence `roundKeys` | is defined as | an initial `key` | followed by | the sequence | with elements of the form `roundKey <<< 1` | where | `roundKey` | draws from | the generated sequence `roundKeys` itself
||||||||||||

Many block ciphers are just variations of the above theme. 
Here's a sample of it in action:

```Xcryptol-session
labs::Language::Basics> encrypt 0x1337c0de 0xdabbad00
0x6157c571
labs::Language::Basics> encrypt 0 0xdabbad00
0xdabbad00
```

The latter shows that you can still write bad crypto with Cryptol!

Notice that both `roundKeys` in `keyExpand` and `roundResults` in
`encrypt` are self-referential sequences, a paradigm that will often
occur when coding cryptography.


## Laziness

Cryptol's evaluation strategy is
[lazy](https://en.wikipedia.org/wiki/Lazy_evaluation)
a.k.a. "call-by-need". I.e., computations are not performed until
necessary. So

```cryptol
abs : [32] -> [32]
abs n = if n >= 0 then n else -n

lazyAbsMin : [32] -> [32] -> [32]
lazyAbsMin x y = if x == 0 then 0 else min (abs x) (abs y)
```

Does not produce an error when `x` is zero, regardless of the value of
`y`. For instance:

```Xcryptol-session-ci-none
labs::Language::Basics> lazyAbsMin 1 (0/0)
```

```Xcryptol-session
division by 0
labs::Language::Basics> lazyAbsMin 0 (0/0)
0x00000000
```

# Conclusion

Go forth and write correct cryptographic algorithms!

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Course README](../../README.md) ||
| [< Interpreter](../Interpreter/Interpreter.md) | **Language Basics** | [CRC >](../CRC/CRC.md) |
||||
|| [+ Style Guide](../../cryptol-style.md) ||
|| [+ Cryptol Demos](../Demos/Cryptol/Demos.md) ||
|| [+ SAW Demos](../Demos/SAW/Demos.md) ||
|| [+ Type Hackery](./IntroTypeHackery.md) ||
