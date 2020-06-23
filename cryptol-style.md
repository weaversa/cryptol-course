Cryptol Style Guide
===================

This document was crafted from [tibbe's Haskell style
guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md). This
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
with *4 spaces*. Indent the `where` keyword two spaces to set it
apart from the rest of the code and indent the definitions in a
`where` clause 2 spaces. Some examples:

```haskell
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
better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator. Don't insert a space after a lambda (the `\`
symbol).

### Type Definitions

It is acceptable to keep type variables, type constraints, and
argument types on one line iff they fit on one line. It is always
acceptable to place type variables, type constraints, and argument
types on separate lines.

```haskell
sayHello :
     {a}
     (fin a) =>
     [a][8] -> [a+7][8]
```

The `:` should always be surrounded by a single space on either
side. This aligns type definitions with value definitions (where the `=`
is also surrounded by a single space on either side). For example,

```haskell
x : [32]
x = 10
y : [4][32]
y = [1, 2, 3, 4]
```

### Type Constraints

Type constraints should always be tupleized. The following is incorrect:

```haskell
fun :
   {a, b}
   fin a => fin b =>
   [a][b] -> [a+b]
```

`fun` should be:

```haskell
fun :
   {a, b}
   (fin a, fin b)
   [a][b] -> [a+b]
```

### Data Declarations

The `=` should always be surrounded by a single space on either
side. This aligns value definitions with type definitions (where the `:`
is also surrounded by a single space on either side). For example,

```haskell
x : [32]
x = 10
y : [4][32]
y = [1, 2, 3, 4]
```

Format records as follows:

```haskell
Person =
    { firstName : String 10
    , lastName  : String 10
    , age       : Integer
    }
```

Align the elements in a list. For example,

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

Optionally, you can skip the first newline. Use your judgement or try
to match the paper document of the specification you're working from.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```


### If-then-else clauses

Align if-then-else clauses like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

The same rule applies in nested where clauses:

```haskell
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

```haskell
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

Comment every top level function (particularly exported functions),
and provide a type signature. The documentation should
give enough information to
apply the function without looking at the function's definition.

When writing a cryptoalgorithm specification in Cryptol, often the
arguments to a function have verbiage in the specification that may be
copied into the function documentation.
This is considered a good practice provided it does not become too verbose.

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces.

```haskell
foo : [32] -> [32]
foo n = n % p
  where
    p = 4294967291  -- Largest 32 bit prime.
```

Naming
------

When implementing cryptoalgorithm specifications, match identifiers to
the case given in the specification.
This helps make the correspondence clear (especially to those more
steeped in cryptoalgorithm specifications).
Should a specification's identifiers include
characters illegal in Cryptol identifiers, underscore is often a
reasonable substitute.

When not constrained by the above it is probably best to follow Haskell's style:

> Use camel case (e.g. `functionName`) when naming functions and upper
> camel case (e.g. `DataType`) when naming data types.

Curried vs. Uncurried Functions
------------------

[Curried functions] (https://en.wikipedia.org/wiki/Currying) are
preferred as they afford [partial application] (https://en.wikipedia.org/wiki/Partial_application) 
and tend to reduce the number of parentheses. However, when writing
Cryptol against a specification document, individual functions should follow
the style of the functions from the document (which will usually be uncurried).




# STOPPED !!!

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type. Function example:

```haskell
-- | Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces. Align
comments for data type definitions. Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### Links

Use in-line links economically. You are encouraged to add links for
API names. It is not necessary to add links for all API names in a
Haddock comment. We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation. For example, write `HttpServer` instead of
`HTTPServer`. Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file. Including this flag in the file itself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

Misc
----

### Point-free style ###

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

### Warnings ###

Code should be compilable with `-Wall -Werror`. There should be no
warnings.
