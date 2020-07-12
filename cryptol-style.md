Cryptol Style Guide
===================

This document was crafted from
[tibbe's Haskell style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md). This
guide covers the major areas of formatting and naming of Cryptol
specifications. That said, the overarching goal of writing a Cryptol
specification is to make it look as much like the corresponding paper
document as possible. So, if there is some clear style in the paper
document, follow that as closely as possible and use this guide to
fill in any gaps.


Formatting
----------

### Line Length

Maximum line length is *70 characters* (which just happens to be the
default `set-fill-column` width in Emacs).

### Indentation

Tabs are illegal. Use spaces for indenting. Indent your code blocks
with *4 spaces*. Indent the `where` keyword two spaces to set it apart
from the rest of the code and indent the definitions in a `where`
clause 2 spaces. Some examples:

```cryptol
sayHello : {a} (fin a) => [a][8] -> [a+7][8]
sayHello name = greeting
  where
    greeting = "Hello, " # name
```

### Blank Lines

One blank line between top-level definitions. No blank lines between
type signatures and function definitions.

### Whitespace

Surround binary operators with a single space on either side. Use your
best judgement for the insertion of spaces around arithmetic
operators, but always be consistent about whitespace on either side of
a binary operator. Don't insert a space after a lambda (the `\`
symbol).

### Type Definitions

It is acceptable to keep type variables, type constraints, and
argument types on one line iff they fit on one line. It is always
acceptable to place type variables, type constraints, and argument
types on separate lines.

```cryptol
sayHello :
     {a}
     (fin a) =>
     [a][8] -> [a+7][8]
```

The `:` should always be surrounded by a single space on either
side. This aligns type definitions with value definitions (where the
`=` is also surrounded by a single space on either side). For example,

```cryptol
x : [32]
x = 10
y : [4][32]
y = [1, 2, 3, 4]
```

### Type Constraints

Type constraints should always be tupleized. The following is
incorrect:

```cryptol
func :
   {a, b}
   fin a => fin b =>
   [a][b] -> [a+b]
```

`func` should be:

```cryptol
func :
   {a, b}
   (fin a, fin b)
   [a][b] -> [a+b]
```

### Data Declarations

The basecase for `=` should be surrounded by a single space on either
side. This aligns value definitions with type definitions (where the
`:` is also surrounded by a single space on either side). For example,

```cryptol
fish : [32]
fish = 10
```

When multiple definitions are in the same where clause, align the
`=` with the longest definition. For example,

```cryptol
horse    : [4][32]
horse    = [1, 2, 3, 4]
elephant : [2]
elephant = [False, True]
```

Format records as follows:

```cryptol
Person =
    { firstName : String 10
    , lastName  : String 10
    , age       : Integer
    }
```

Align the elements in a list. For example,

```cryptol
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Optionally, you can skip the first newline. Use your judgement or try
to match the specification you're working from.

```cryptol
directions = [ North
             , East
             , South
             , West
             ]
```

### Properties

Shortcircuit logical operators are preferred to if-then-else in
properties. For example,

```cryptol
property myProperty x =
    x != 0 ==> (100/x) <= 100
```

is superior to

```cryptol
property myProperty x =
    if x != 0
    then (100/x) <= 100
    else True
```

### If-then-else clauses

Align if-then-else clauses like you would normal expressions:

```cryptol
foo = if ...
      then ...
      else ...
```

The same rule applies in nested where clauses:

```cryptol
foo a b = c
  where
    c = if a == 0x0000
        then d
          where
            d = if b == 0x0000
                then 0x1000
                else 0x0010
        else 0x0100
```

### Where clauses

Align the `=` symbols in a where clause when it helps readability.

```cryptol
foo = ...
  where
    cat   = ...
    fish  = ...
    dog   = ...
    horse = ...
```


Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Comment every top level function (particularly exported functions)
using [docstring comments](https://en.wikipedia.org/wiki/Docstring),
and provide a type signature. The documentation should give enough
information to apply the function without looking at the function's
definition.

When writing a cryptographic algorithm specification in Cryptol, often
the arguments to a function have verbiage in the specification that
may be copied into the function documentation. This is considered a
good practice provided it does not become too verbose.

### End-of-Line Comments

Separate end-of-line comments from the code using two spaces.

```cryptol
/**
 * Here is a docstring comment for the function named foo.
 * This comment will appear in the Cryptol interpreter
 * when you type `:help foo`.
 */
foo : [32] -> [32]
foo n = n % p
  where
    p = 4294967291  // Largest 32 bit prime.
```


Naming
------

When implementing cryptographic algorithm specifications, match
identifiers to the case given in the specification. This helps make
the correspondence clear (especially to those more steeped in
cryptographic algorithm specifications). Should a specification's
identifiers include characters illegal in Cryptol identifiers,
underscore is often a reasonable substitute.

When not constrained by the above it is probably best to follow
Haskell's style:

> Use camel case (e.g. `functionName`) when naming functions and upper
> camel case (e.g. `DataType`) when naming data types.

As well, an apostrophe (`'`) can be appended to a function name to
denote a relationship.

```cryptol
encrypt key plainText = f  key plainText

decrypt key plainText = f' key plainText
```

Curried vs. Uncurried Functions
-------------------------------

[Curried functions] (https://en.wikipedia.org/wiki/Currying) are
preferred as they afford [partial application]
(https://en.wikipedia.org/wiki/Partial_application) and tend to reduce
the number of parentheses. However, when writing Cryptol against a
specification document, individual functions should follow the style
of the functions from the document (which will usually be uncurried).


Argument Order
--------------

Order arguments so that partial application is most advantageous. For
example,

```cryptol
encrypt key plainText = ...
```

is superior to

```cryptol
encrypt plainText key = ...
```

since it is easy to see the utility of `encrypt key`
as a function in its own right.


### Warnings ###

All Cryptol specifications should load into the Cryptol interpreter
without warnings.
