# Introduction

This lab provides a series of exercises focused on a cryptographic
algorithm called Salsa20, by Daniel J. Bernstein.

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed,
  * this module to load successfully, and
  * an editor for completing the exercises in this file.
  
You'll also need experience with
  * loading modules and evaluating functions in the interpreter,
  * Cryptol's sequence types,
  * the `:prove` command,
  * manipulating sequences using `#`, `split`, `join`, `take`, `drop`,
    `map`, `iterate`, `transpose`, and `reverse`,
  * writing functions and properties,
  * sequence comprehensions, and
  * logical, comparison, arithmetic, indexing, and conditional
    operators.

## Skills You'll Learn

By the end of this lab you will have implemented Salsa20.

You'll also gain experience with
  * type parameters and type constraints,
  * demoting types variables to value variables,
  * manipulating sequences,
  * writing functions and properties, and
  * navigating some nuances of Cryptol's type checking system.
  
## Load This Module

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming)
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```Xcryptol-session
Loading module Cryptol
Cryptol> :m labs::Salsa20::Salsa20Answers
Loading module Cryptol
Loading module labs::Salsa20::Salsa20Answers
```

We start by defining a new module for this lab:

```cryptol
module labs::Salsa20::Salsa20Answers where
```

You do not need to enter the above into the interpreter; the previous 
`:m ...` command loaded this literate Cryptol file automatically.
In general, you should run `Xcryptol-session` commands in the 
interpreter and leave `cryptol` code alone to be parsed by `:m ...`.

# Salsa20

Salsa20 is a cryptographic algorithm created by Daniel
J. Bernstein. The specification is available
[here](https://cr.yp.to/snuffle/spec.pdf), but also provided in this
repository [here](./Salsa20Spec.pdf) for ease of access. The
specification document describes Salsa20 as well as how to use it as a
[stream cipher](https://en.wikipedia.org/wiki/Stream_cipher) in
[counter
mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Counter_(CTR)).

This lab goes through the [Salsa20 specification
document](./Salsa20Spec.pdf) section by section, showing how to write a
fairly pedantic Cryptol specification of Salsa20. We recommend you
either print the specification document, or have this lab and the
specification document open side-by-side.

Now we can begin to dig into the specification document!


## 1 Introduction

The last paragraph of the introduction defines a **byte** to mean an
element of {0,1,...,255} and says that

> there are many common ways to represent a byte.

This is an opportunity for us to specify a representation in our
specification. Here, we define `Bytes` as a new type synonym for a
sequence of `n` 8-bit words.

```cryptol
type Bytes n = [n][8]
```


## 2 Words

This section defines **word** to be an element of {0,1,...,2^32 -
1}. Similarly to `Bytes` above, we define `Words` as a new type
synonym for a sequence of `n` 32-bit words.

```cryptol
type Words n = [n][32]
```

The specification then goes on to give an example of how words are
expressed in hexadecimal throughout the rest of the document. Here, we
show that Cryptol natively agrees with this expression by encoding the
example as a property.

```cryptol
property hexadecimalProp =
    (0xc0a8787e == 12 * (2^^28)
                 +  0 * (2^^24)
                 + 10 * (2^^20)
                 +  8 * (2^^16)
                 +  7 * (2^^12)
                 +  8 * (2^^ 8)
                 +  7 * (2^^ 4)
                 + 14 * (2^^ 0)) /\
    (0xc0a8787e == 3232266366)
```

Let's prove `hexadecimalProp`:

```Xcryptol-session
labs::Salsa20::Salsa20> :prove hexadecimalProp
Q.E.D.
(Total Elapsed Time: 0.006s, using "Z3")
```

Here we see [Q.E.D.](https://en.wikipedia.org/wiki/Q.E.D.) which means
the property is true. As you work through the rest of this document,
please try and `:prove` the property statements littered throughout to
help you check your work.

This section then defines the **sum** of two words and provides an
example. Here, we show that Cryptol's `+` operator agrees with this
definition of sum by encoding the example as a property.

```cryptol
property sumProp =
    0xc0a8787e + 0x9fd1161d == 0x60798e9b
```

Next, **exclusive-or** and **c-bit left rotation** are defined and
examples provided. One small syntactic change is needed, namely, the
exclusive-or symbol in Cryptol is `^` rather than ⊕. So, in Cryptol,
these examples become:

```cryptol
property exclusiveOrProp =
    0xc0a8787e ^ 0x9fd1161d == 0x5f796e63

property LeftRotationProp =
    0xc0a8787e <<< 5 == 0x150f0fd8
```


## 3 The quarterround function


### Inputs and outputs

This section begins by giving the type of the quarterround function as:

> If y is a 4-word sequence then quarterround(y) is a 4-word sequence.

In Cryptol, we would write this type as:

```cryptol
quarterround : Words 4 -> Words 4
```


### Definition

This section then goes on to define quarterround. Two syntactic changes are
required, namely, sequences in Cryptol are book-ended by `[]` rather
than `()` and commas are not needed after statements in a where
clause.

```cryptol
quarterround [y0, y1, y2, y3] = [z0, z1, z2, z3] where
    z1 = y1 ^ ((y0 + y3) <<< 7)
    z2 = y2 ^ ((z1 + y0) <<< 9)
    z3 = y3 ^ ((z2 + z1) <<< 13)
    z0 = y0 ^ ((z3 + z2) <<< 18)
```

### Examples

Example input-output pairs are provided. Cryptol can make use of these
to provide evidence that quarterround was implemented correctly. Here
we create a property that will verify whether quarterround works
correctly on the provided example pairs.

```cryptol
property quarterroundExamplesProp =
    (quarterround [0x00000000, 0x00000000, 0x00000000, 0x00000000]
               == [0x00000000, 0x00000000, 0x00000000, 0x00000000]) /\
    (quarterround [0x00000001, 0x00000000, 0x00000000, 0x00000000]
               == [0x08008145, 0x00000080, 0x00010200, 0x20500000]) /\
    (quarterround [0x00000000, 0x00000001, 0x00000000, 0x00000000]
               == [0x88000100, 0x00000001, 0x00000200, 0x00402000]) /\
    (quarterround [0x00000000, 0x00000000, 0x00000001, 0x00000000]
               == [0x80040000, 0x00000000, 0x00000001, 0x00002000]) /\
    (quarterround [0x00000000, 0x00000000, 0x00000000, 0x00000001]
               == [0x00048044, 0x00000080, 0x00010000, 0x20100001]) /\
    (quarterround [0xe7e8c006, 0xc4f9417d, 0x6479b4b2, 0x68c67137]
               == [0xe876d72b, 0x9361dfd5, 0xf1460244, 0x948541a3]) /\
    (quarterround [0xd3917c5b, 0x55f1c407, 0x52a58a7a, 0x8f887a3b]
               == [0x3e2f308c, 0xd90a8f36, 0x6ab2a923, 0x2883524c])
```

This is an excellent opportunity to check that the quarterround
function we've specified in Cryptol does indeed work correctly on
these examples.

```Xcryptol-session
labs::Salsa20::Salsa20> :prove quarterroundExamplesProp
Q.E.D.
(Total Elapsed Time: 0.005s, using "Z3")
```

### Comments

There is one last paragraph in this section that may not seem
significant at first glance. This paragraph ends with

> ...so the entire function is invertible.

This claim, if true, would make the quarterround function itself
collision-free; an important property in cryptography. However, the
author does little to convince us that this is true. Fortunately, we
can prove this using Cryptol.

You may be wondering, what does it mean for a function to be
invertible? Well, it means that a function (say, quarterround) has an
inverse function (call it quarterround') such that for all possible
values of y, quarterround' (quarterround y) == y. Unfortunately the
author didn't provide us with such an inverse function. Now, we could
attempt to create it, but there is a much simpler solution here!

A function is invertible if
 * each input maps to a unique output (collision free) and
 * every element in the range of the function can be mapped to by some
   input (no output is missing).

In mathematics, this type of function would be called
bijective. Bijective functions are both injective and surjective
(one-to-one and onto). So, to prove that quarterround is invertible we
can prove that it is bijective. This can be done by showing that
quarterround is both injective and surjective. Now, since the domain
and range of quarterround are the same (`Word 4 -> Word 4`), we know
that, if the function is injective, it must also be surjective. Hence,
we only need to prove that quarterround is injective.

Wikipedia defines an [injective
function](https://en.wikipedia.org/wiki/Bijection,_injection_and_surjection#Injection)
as:

![](https://render.githubusercontent.com/render/math?math=\forall%20x,x'%20\in%20X,%20x%20\neq%20x'%20\Rightarrow%20f(x)%20\neq%20f(x'))

For the non-mathematician, this says that a function is injective if
every pair of different inputs causes the function to produce
different outputs. We can now encode this (almost verbatim) into a
Cryptol property.

```cryptol
property quarterroundIsInjectiveProp x x' =
    x != x' ==> quarterround x != quarterround x'
```

And then prove that the property is true.

```Xcryptol-session
labs::Salsa20::Salsa20> :prove quarterroundIsInjectiveProp
Q.E.D.
(Total Elapsed Time: 0.430s, using "Z3")
```

It's worth noting here that Cryptol and associated theorem provers are
doing some very heavy lifting behind the scenes. Without an automated
theorem prover, the best one could do is run some tests. Cryptol does
support automated testing with its `:check` command.

```Xcryptol-session
labs::Salsa20::Salsa20> :check quarterroundIsInjectiveProp
Using random testing.
Passed 100 tests.
Expected test coverage: 0.00% (100 of 2^^256 values)
```

This command provides a quick check on properties and is useful if
`:prove` is taking too long or fails for some reason. But the time it
would take to ensure there are no "needles in the haystack" is more
than the number of seconds left before the [Sun swallows the
Earth](https://en.wikipedia.org/wiki/Future_of_Earth). However, if
you'd prefer to try, Cryptol's `:exhaust` is the command to use.

```Xcryptol-session ci-none
labs::Salsa20::Salsa20> 2^^256 : Integer
115792089237316195423570985008687907853269984665640564039457584007913129639936
labs::Salsa20::Salsa20> :exhaust quarterroundIsInjectiveProp
Using exhaustive testing.
Testing...     0%
```

While you wait, you should consider just how great automated theorem
provers have become. Next, appreciate all the work the folks at Galois
have put into Cryptol over the last 20 years...you may even consider
sending a thank you note to Évariste (evariste@galois.com). After
that, why not go on to the next section? And then the next, and the
next...oh just hit Ctrl-C already.


## 4 The rowround function

From here on out, this lab requires you to write the rest of Salsa20
by following the specification document. This may seem to you that
we've become lazy, but we see it as important that you have the
opportunity to get your hands dirty. The lab will provide some
commentary, type definitions, function stubs, and the example
input-output pairs as property statements so you can check your work.


### Inputs and outputs

```cryptol
rowround : Words 16 -> Words 16
```


### Definition

**EXERCISE**: Here we provide a skeleton for `rowround`. Please
replace the `undefined` symbols with the appropriate logic as given in
the Salsa20 specification. You'll know you've gotten it right when it
looks like the specification and when `:prove rowroundExamplesProp`
gives `Q.E.D`.

```cryptol
rowround [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] =
    [z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15]
  where
    [ z0,  z1,  z2,  z3] = quarterround [ y0,  y1,  y2,  y3]
    [ z5,  z6,  z7,  z4] = quarterround [ y5,  y6,  y7,  y4]
    [z10, z11,  z8,  z9] = quarterround [y10, y11,  y8,  y9]
    [z15, z12, z13, z14] = quarterround [y15, y12, y13, y14]
```


### Examples

```cryptol
property rowroundExamplesProp =
    (rowround [0x00000001, 0x00000000, 0x00000000, 0x00000000,
               0x00000001, 0x00000000, 0x00000000, 0x00000000,
               0x00000001, 0x00000000, 0x00000000, 0x00000000,
               0x00000001, 0x00000000, 0x00000000, 0x00000000]
           == [0x08008145, 0x00000080, 0x00010200, 0x20500000,
               0x20100001, 0x00048044, 0x00000080, 0x00010000,
               0x00000001, 0x00002000, 0x80040000, 0x00000000,
               0x00000001, 0x00000200, 0x00402000, 0x88000100]) /\
    (rowround [0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
               0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
               0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
               0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]
           == [0xa890d39d, 0x65d71596, 0xe9487daa, 0xc8ca6a86,
               0x949d2192, 0x764b7754, 0xe408d9b9, 0x7a41b4d1,
               0x3402e183, 0x3c3af432, 0x50669f96, 0xd89ef0a8,
               0x0040ede5, 0xb545fbce, 0xd257ed4f, 0x1818882d])
```


### Comments

The comments in this section hint at an optimized way to perform
`rowround`. Here is one such optimized function:

```cryptol
rowroundOpt : Words 16 -> Words 16
rowroundOpt ys =
    join [ quarterround (yi <<< i) >>> i
         | yi <- split ys
         | (i : [2])  <- [0 .. 3] ]
```

**EXERCISE**: Here we want to prove that for all inputs, `rowroundOpt` is
equal to `rowround`. Please replace the `False` symbol below with such
a statement and then prove the property in the interpreter. It's not
necessary to go through this exercise to create a complete Salsa20
specification, but it's a good opportunity here to learn more about
Cryptol's properties.

```cryptol
property rowroundOptProp ys = rowroundOpt ys == rowround ys
```

## 4 The columnround function


### Inputs and outputs

```cryptol
columnround : Words 16 -> Words 16
```


### Definition

**EXERCISE**: Here we provide a skeleton for `columnround`. Please
replace the `undefined` symbols with the appropriate logic as given in
the Salsa20 specification. You'll know you've gotten it right when it
looks like the specification and when `:prove columnroundExamplesProp`
gives `Q.E.D`.

```cryptol
columnround [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15] =
    [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15]
  where
    [ y0,  y4,  y8, y12] = quarterround [ x0,  x4,  x8, x12]
    [ y5,  y9, y13,  y1] = quarterround [ x5,  x9, x13,  x1]
    [y10, y14,  y2,  y6] = quarterround [x10, x14,  x2,  x6]
    [y15,  y3,  y7, y11] = quarterround [x15,  x3,  x7, x11]
```


### Examples

```cryptol
property columnroundExamplesProp =
    (columnround [0x00000001, 0x00000000, 0x00000000, 0x00000000,
                  0x00000001, 0x00000000, 0x00000000, 0x00000000,
                  0x00000001, 0x00000000, 0x00000000, 0x00000000,
                  0x00000001, 0x00000000, 0x00000000, 0x00000000]
              == [0x10090288, 0x00000000, 0x00000000, 0x00000000,
                  0x00000101, 0x00000000, 0x00000000, 0x00000000,
                  0x00020401, 0x00000000, 0x00000000, 0x00000000,
                  0x40a04001, 0x00000000, 0x00000000, 0x00000000]) /\
    (columnround [0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
                  0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
                  0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
                  0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a]
              == [0x8c9d190a, 0xce8e4c90, 0x1ef8e9d3, 0x1326a71a,
                  0x90a20123, 0xead3c4f3, 0x63a091a0, 0xf0708d69,
                  0x789b010c, 0xd195a681, 0xeb7d5504, 0xa774135c,
                  0x481c2027, 0x53a8e4b5, 0x4c1f89c5, 0x3f78c9c8])
```


### Comments

The comments in this section say that `columnround` is:

> One can visualize the input (x0, x1, . . . , x15) as a square
matrix...The columnround function is, from this perspective, simply
the transpose of the `rowround` function

**EXERCISE**: Here is another property we can prove using Cryptol. The
author claims that rejiggering the input and outputs of `rowround` (in
a special way) will cause `rowround` to produce results identical to
`columnround`. To rejigger we

  * transform the 16-element input sequence into a 4 by 4 element
sequence,
  * transpose this matrix, and
  * transform the transposed 4 by 4 matrix back into a 16-element
sequence.

Luckily Cryptol provides `split`, `transpose`, and `join` to perform
these three operations. Please replace the `undefined` symbol below
with an appropriate rejigger function and use it to prove that
`columnround` is the transpose of `rowround`.

```cryptol
property columnroundIsTransposeOfRowround ys =
    columnround ys == rejigger (rowround (rejigger ys))
  where
    rejigger a = join (transpose (split`{4} a))
```


## 6 The doubleround function


### Inputs and outputs

```cryptol
doubleround : Words 16 -> Words 16
```


### Definition

**EXERCISE**: Here we provide a skeleton for `doubleround`. Please
replace the `undefined` symbol with the appropriate logic as given in
the Salsa20 specification. You'll know you've gotten it right when it
looks like the specification and when `:prove doubleroundExamplesProp`
gives `Q.E.D`.

```cryptol
doubleround xs = rowround (columnround xs)
```


### Examples

```cryptol
property doubleroundExamplesProp =
    (doubleround [0x00000001, 0x00000000, 0x00000000, 0x00000000,
                  0x00000000, 0x00000000, 0x00000000, 0x00000000,
                  0x00000000, 0x00000000, 0x00000000, 0x00000000,
                  0x00000000, 0x00000000, 0x00000000, 0x00000000]
              == [0x8186a22d, 0x0040a284, 0x82479210, 0x06929051,
                  0x08000090, 0x02402200, 0x00004000, 0x00800000,
                  0x00010200, 0x20400000, 0x08008104, 0x00000000,
                  0x20500000, 0xa0000040, 0x0008180a, 0x612a8020]) /\
    (doubleround [0xde501066, 0x6f9eb8f7, 0xe4fbbd9b, 0x454e3f57,
                  0xb75540d3, 0x43e93a4c, 0x3a6f2aa0, 0x726d6b36,
                  0x9243f484, 0x9145d1e8, 0x4fa9d247, 0xdc8dee11,
                  0x054bf545, 0x254dd653, 0xd9421b6d, 0x67b276c1]
              == [0xccaaf672, 0x23d960f7, 0x9153e63a, 0xcd9a60d0,
                  0x50440492, 0xf07cad19, 0xae344aa0, 0xdf4cfdfc,
                  0xca531c29, 0x8e7943db, 0xac1680cd, 0xd503ca00,
                  0xa74b2ad6, 0xbc331c5c, 0x1dda24c7, 0xee928277])
```


### Comments

There don't seem to be any interesting properties to prove here, so
we'll move on.


## 7 The littleendian function


### Inputs and outputs

```cryptol
littleendian : Bytes 4 -> [32]
```


### Definition

**EXERCISE**: Here we provide a skeleton for `littleendian`. Please
replace the `undefined` symbol with the appropriate logic as given in
the Salsa20 specification. You'll know you've gotten it right when
`:prove littleendianExamplesProp` gives `Q.E.D`.

This one is a little tricky because the elegant solution does not look
like the solution in the paper document. This is because, for example,
`b0` (an 8-bit value) added to anything can never produce a 32-bit
result --- Cryptol enforces that it can only add, multiply, subtract,
etc. n-bit things to other n-bit things. So, one solution would be to
create new 32-bit variables that are `b0` through `b3` each padded
with 24 zeroes, and then do the arithmetic in the
specification. However, there is a much simpler solution. Good luck!

```cryptol
littleendian [b0, b1, b2, b3] = join [b3, b2, b1, b0]
```


### Examples

```cryptol
property littleendianExamplesProp =
    (littleendian [  0,   0,   0,   0] == 0x00000000) /\
    (littleendian [ 86,  75,  30,   9] == 0x091e4b56) /\
    (littleendian [255, 255, 255, 250] == 0xfaffffff)
```


### Comments

As before, the author notes that this function is invertible and does
not provide the inverse. However, this undefined littleendian inverse
function is used in the next section! So, we will have to define it
here. There's one hiccup though, the littleendian function works on
4-byte sequences, but littleendian inverse has to work on 4-byte and
8-byte sequences, so we've defined the type of the function to work on
n-byte sequences.

**EXERCISE**: Here we provide a skeleton for `littleendian'`, the
inverse to `littleendian`. Please replace the `undefined` symbol with
the appropriate logic such that `:prove littleendianInverseProp` gives
`Q.E.D`.

```cryptol
littleendian' : {n} (fin n) => [n*8] -> Bytes n
littleendian' w = reverse (split w)
```

```cryptol
property littleendianInverseProp b = littleendian' (littleendian b) == b
```


## The Salsa20 hash function

From [The Salsa20 core](http://cr.yp.to/salsa20.html) (Bernstein):

> I originally introduced the Salsa20 core as the "Salsa20 hash
function," but this terminology turns out to confuse people who think
that "hash function" means "collision-resistant compression function."
The Salsa20 core does not compress and is not collision-resistant.

So, given that the original author has reconsidered this nomenclature,
even though the specification itself still refers to the Salsa20 core
as a hash function and has not been amended, here we choose to go with
the times and name this function `Salsa20Core`.


### Inputs and outputs

```cryptol
Salsa20Core : Bytes 64 -> Bytes 64
```


### Definition

This function is more complicated than ones we've seen so far, but
operation can be simply described in four steps:

  1. `map littleendian` over chunks of the 64-byte sequence `x`,
     creating a 16-word sequence `xs`.
  2. `iterate` doubleround 10 times, starting with the 16 words from
     step 1.
  3. Add together the values produced in step 1 to step 2.
  4. `map littelendian'` over 16-word sequence from step 3,
     creating a 64-byte sequence `x'`.

**EXERCISE**: Here we provide a skeleton for `Salsa20Core`. Please
replace the `undefined` symbol with the appropriate logic such that
`:prove Salsa20CoreExamplesProp` gives `Q.E.D`.

```cryptol
Salsa20Core x = x'
  where
    //Step 1
    xs    = map littleendian (split x)
    //Step 2
    zs    = (iterate doubleround xs)@10
    //Step 3
    xspzs = xs + zs
    //Step 4
    x'    = join (map littleendian' xspzs)
```


### Examples

```cryptol
property Salsa20CoreExamplesProp =
    (Salsa20Core [  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0]
              == [  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0]) /\
    (Salsa20Core [211, 159,  13, 115,  76,  55,  82, 183,   3, 117, 222,  37, 191, 187, 234, 136,
                   49, 237, 179,  48,   1, 106, 178, 219, 175, 199, 166,  48,  86,  16, 179, 207,
                   31, 240,  32,  63,  15,  83,  93, 161, 116, 147,  48, 113, 238,  55, 204,  36,
                   79, 201, 235,  79,   3,  81, 156,  47, 203,  26, 244, 243,  88, 118, 104,  54]
              == [109,  42, 178, 168, 156, 240, 248, 238, 168, 196, 190, 203,  26, 110, 170, 154,
                   29,  29, 150,  26, 150,  30, 235, 249, 190, 163, 251,  48,  69, 144,  51,  57,
                  118,  40, 152, 157, 180,  57,  27,  94, 107,  42, 236,  35,  27, 111, 114, 114,
                  219, 236, 232, 135, 111, 155, 110,  18,  24, 232,  95, 158, 179,  19,  48, 202]) /\
    (Salsa20Core [ 88, 118, 104,  54,  79, 201, 235,  79,   3,  81, 156,  47, 203,  26, 244, 243,
                  191, 187, 234, 136, 211, 159,  13, 115,  76,  55,  82, 183,   3, 117, 222,  37,
                   86,  16, 179, 207,  49, 237, 179,  48,   1, 106, 178, 219, 175, 199, 166,  48,
                  238,  55, 204,  36,  31, 240,  32,  63,  15,  83,  93, 161, 116, 147,  48, 113]
              == [179,  19,  48, 202, 219, 236, 232, 135, 111, 155, 110,  18,  24, 232,  95, 158,
                   26, 110, 170, 154, 109,  42, 178, 168, 156, 240, 248, 238, 168, 196, 190, 203,
                   69, 144,  51,  57,  29,  29, 150,  26, 150,  30, 235, 249, 190, 163, 251,  48,
                   27, 111, 114, 114, 118,  40, 152, 157, 180,  57,  27,  94, 107,  42, 236,  35])
```


### Comments

Nothing of consequence


## The Salsa20 expansion function


### Inputs and outputs

This is our first
[polymorphic](https://en.wikipedia.org/wiki/Polymorphism_(computer_science))
function. In case you need a refresher, a polymorphic function is one
that can take more than one type of input. In this case, we have to
take two different types of `k` --- one that is a 32-byte sequence and
one that is a 16-byte sequence. Here we create a _type variable_ `a`
such that when `a` is one, the function expects a 16-byte `k` and when
it's two, the function expects a 32-byte `k`. We also constrain `k`
(using _type constraints_) to only be able to take on the value one or
two.

```cryptol
Salsa20Expansion :
    {a}
    (a >= 1, 2 >= a) =>
    Bytes (16*a) -> Bytes 16 -> Bytes 64
```


### Definition

This definition is even trickier than the last because we have to
account for the two different sizes of `k`. If `a` is two then we have
to call Salsa20Core(s0, k0, s1, n, s2, k1, s3) where k0 are the low 16
bytes of k and k1 are the high 16 bytes of k. In the case where `a` is
one then we have to call Salsa20Core(t0, k0, t1, n, t2, k0, t3), and
k1 is undefined. There are a few different ways to define k0 and
k1. This is, admittedly, often the most frustrating part of
specifying `Salsa20Expansion`. If you're up for figuring it out,
please do, but you won't be faulted for skipping the headache and
checking the answer key (3 different definitions are provided there).

The notation the author used here is also a little strange --- you'll
notice that Salsa20Core doesn't actually take 7 different values as
input. The author means, for example, when `a` is one that t0, k, t1,
n, t2, k, and t3 should be concatenated together to make a sequence of
64-bytes.

As well, you'll need to access the type variable `a`. Here we quote
from Section 1.19.2 from [Programming
Cryptol](https://github.com/GaloisInc/cryptol/blob/master/docs/ProgrammingCryptol.pdf)

> You have seen, in the discussion of type variables above, that
> Cryptol has two kinds of variables --- type variables and value
> variables. Type variables normally show up in type signatures, and
> value variables normally show up in function definitions. Sometimes
> you may want to use a type variable in a context where value
> variables would normally be used. To do this, use the backtick
> character `` ` ``. The definition of the built-in length function is
> a good example of the use of backtick:

```comment
length : {n, a, b} (fin n, Literal n b) => [n]a -> b
length _ = `n
```

You'll find sigma and tau defined in a kind of fancy way in the
Comments section below.

**EXERCISE**: Here we provide a skeleton for `Salsa20Expansion`. Please
fill in the definition of the function such that `:prove
Salsa20ExpansionExamplesProp` gives `Q.E.D`. You'll likely want to add
a `where` clause as well.

```cryptol
Salsa20Expansion k n = z
  where
    x = if (`a : [2]) == 1
        then t0 # k0 # t1 # n # t2 # k0 # t3
        else s0 # k0 # s1 # n # s2 # k1 # s3
    z = Salsa20Core x
    (k0 # k1) = k # undefined
    //[k0, k1] = split (k # undefined)
    //(k0, k1) = (take k, drop k)
```

This last definition for `k0` and `k1` isn't as nice because `k1 ==
k0` when `a` is one (not that it really makes any difference).


### Examples

```cryptol
property Salsa20ExpansionExamplesProp =
    (Salsa20Expansion (k0#k1) n ==
    [ 69,  37,  68,  39,  41,  15, 107, 193, 255, 139, 122,   6, 170, 233, 217,  98,
      89, 144, 182, 106,  21,  51, 200,  65, 239,  49, 222,  34, 215, 114,  40, 126,
     104, 197,   7, 225, 197, 153,  31,   2, 102,  78,  76, 176,  84, 245, 246, 184,
     177, 160, 133, 130,   6,  72, 149, 119, 192, 195, 132, 236, 234, 103, 246,  74]) /\
    (Salsa20Expansion k0 n ==
    [ 39, 173,  46, 248,  30, 200,  82,  17,  48,  67, 254, 239,  37,  18,  13, 247,
     241, 200,  61, 144,  10,  55,  50, 185,   6,  47, 246, 253, 143,  86, 187, 225,
     134,  85, 110, 246, 161, 163,  43, 235, 231,  94, 171,  51, 145, 214, 112,  29,
      14, 232,   5,  16, 151, 140, 183, 141, 171,   9, 122, 181, 104, 182, 177, 193])
  where
    k0 = [1 .. 16]
    k1 = [201 .. 216]
    n  = [101 .. 116]
```


### Comments

The *Definition* section of the Salsa20 specification gives integer
values for sigma and tau. While it's possible to write them out that
way, it looks much nicer to define them as below, corresponding to the
ASCII strings given in the *Comments* section of the specification.

```cryptol
[s0, s1, s2, s3] = split "expand 32-byte k"
[t0, t1, t2, t3] = split "expand 16-byte k"
```


## The Salsa20 encryption function


### Inputs and outputs

Again we have a polymorphic function on `k`. Also notice the type
constraint levied on `l`. This really shows Cryptol's strength as a
specification language. Cryptol isn't powerful enough to actually run
`2^^70` bytes through this function, but the constraint can still be
expressed, if only for documentation purposes.

```cryptol
Salsa20Encrypt :
    {a, l}
    (a >= 1, 2 >= a, l <= 2^^70) =>
    Bytes (16*a) -> Bytes 8 -> Bytes l -> Bytes l
```


### Definition

The only real trouble you may have with this function (and believe
that expert cryptographers have messed it up) is that `i` needs to be
expressed in little-endian, and Cryptol's bitvectors are natively in
big-endian. So, feel free to use the overloaded `littleendian'`
function from Section 7.

Some hints:

  * Where the specification says "truncate" think `take`.
  * Don't be afraid to `take` from an implicitly constructed `2^^70`
    byte sequence.
  * Salsa20Expansion returns a sequence of 64-bytes, so a `join` is
    needed if you want to create the sequence of `2^^70` bytes.
  * `v` and `i` should be concatenated to be passed to
    `Salsa20Expansion`.

```cryptol
Salsa20Encrypt k v m = c
  where
    c = m ^ take (join [ Salsa20Expansion k (v # littleendian' i)
                       | i <- [0 .. 2^^64-1 ] ])
```


### Examples?

It's a little strange to get to the main encryption function in a
specification and find that the test vectors are missing. It's more
often the other way around where test vectors are provided for the main
function but not for anything else.

Turns out there were official test vectors on
[ECRYPT](http://www.ecrypt.eu.org/stream/svn/viewcvs.cgi/ecrypt/trunk/submissions/salsa20/full/verified.test-vectors?logsort=rev&rev=210&view=markup),
but that the link is now defunct. It doesn't seem to be that big of a
loss because the test vectors really didn't test all the functionality
of Salsa20Encrypt because they _all_ took a message of entirely zeroes
as input. This makes it difficult to know if you're processing an
actual message correctly.

So, here we decided to take two of the test vectors from the defunct
site and rework them with a random message, simply so that you can
test your `Salsa20Encrypt` function against them.

```cryptol
property Salsa20EncryptExamplesProp =
    (Salsa20Encrypt [0x00, 0x53, 0xa6, 0xf9, 0x4c, 0x9f, 0xf2, 0x45,
                     0x98, 0xeb, 0x3e, 0x91, 0xe4, 0x37, 0x8a, 0xdd]
                    [0x0d, 0x74, 0xdb, 0x42, 0xa9, 0x10, 0x77, 0xde]
                    [0x46, 0x82, 0x39, 0x77, 0xf3, 0x81, 0xae, 0xd3,
                     0x53, 0x45, 0x2c, 0x2f, 0xf2, 0x10, 0xfd, 0xfa,
                     0x11, 0x44, 0x74, 0x3d, 0x23, 0xf1, 0xf0, 0xdb,
                     0x6e, 0x99, 0x86, 0x73, 0xba, 0x23, 0xee, 0xfb,
                     0xff, 0xde, 0xc0, 0x35, 0x03, 0x31, 0x47, 0x70,
                     0x6d, 0x58, 0x38, 0x88, 0x2d, 0xa7, 0x66, 0xb8,
                     0x2d, 0xb5, 0x88, 0xa0, 0x19, 0x76, 0x92, 0xcd,
                     0x32, 0x24, 0x5b, 0xcc, 0x9d, 0xba, 0x2d, 0x2e]
                 == [0x43, 0x63, 0xde, 0xc9, 0x45, 0x16, 0x77, 0x4a,
                     0x36, 0x2e, 0xdf, 0x53, 0xe9, 0x87, 0x75, 0xfc,
                     0x62, 0x19, 0x7f, 0xad, 0x19, 0x91, 0xf7, 0x66,
                     0x5c, 0x00, 0xa1, 0x9c, 0x04, 0x38, 0xe0, 0xd1,
                     0x7e, 0xe9, 0x01, 0x9b, 0x2a, 0x25, 0xd4, 0xda,
                     0xee, 0xf0, 0x19, 0xfd, 0x76, 0x49, 0x6d, 0xbe,
                     0xe0, 0xa1, 0x0d, 0xfa, 0x7e, 0x92, 0xf5, 0xce,
                     0xd9, 0xdc, 0xa8, 0xdd, 0xd6, 0xe2, 0x61, 0x94]) /\
    (Salsa20Encrypt [0x0a, 0x5d, 0xb0, 0x03, 0x56, 0xa9, 0xfc, 0x4f,
                     0xa2, 0xf5, 0x48, 0x9b, 0xee, 0x41, 0x94, 0xe7,
                     0x3a, 0x8d, 0xe0, 0x33, 0x86, 0xd9, 0x2c, 0x7f,
                     0xd2, 0x25, 0x78, 0xcb, 0x1e, 0x71, 0xc4, 0x17]
                    [0x1f, 0x86, 0xed, 0x54, 0xbb, 0x22, 0x89, 0xf0]
                    [0x5f, 0xb7, 0x9d, 0xab, 0xec, 0x06, 0x21, 0xd8,
                     0x76, 0x1e, 0x37, 0x00, 0x86, 0xfe, 0x0a, 0xea,
                     0x0b, 0x4e, 0x92, 0x19, 0x27, 0x1f, 0x6a, 0x24,
                     0xda, 0x29, 0xe6, 0x87, 0x9b, 0x8b, 0x8a, 0x72,
                     0xb7, 0xa2, 0xae, 0x2b, 0x52, 0x9e, 0x82, 0x15,
                     0x89, 0xd0, 0x0a, 0xf9, 0x3b, 0xcf, 0x9e, 0x4f,
                     0x76, 0x6b, 0xff, 0x8b, 0x29, 0x57, 0xd5, 0x38,
                     0x7d, 0x8c, 0x22, 0x88, 0x38, 0x18, 0x26, 0x4c]
                 == [0x60, 0x5f, 0xc0, 0xf0, 0x5d, 0x90, 0x2b, 0x5a,
                     0x3e, 0x15, 0x69, 0x6f, 0xc8, 0x68, 0x50, 0xae,
                     0x6b, 0x99, 0x37, 0x5c, 0x26, 0x79, 0x25, 0x59,
                     0xba, 0x9c, 0xad, 0x81, 0x8b, 0x81, 0xbd, 0x8d,
                     0x6b, 0x54, 0x13, 0xce, 0x9c, 0xa1, 0xca, 0x93,
                     0x33, 0xa7, 0xd7, 0xa2, 0x7f, 0x26, 0xc8, 0x0b,
                     0x92, 0x61, 0x75, 0x4d, 0x71, 0x56, 0xc0, 0x65,
                     0xc4, 0x83, 0x20, 0xda, 0x13, 0x7c, 0x66, 0x6f])
```


### Comments

Well, if you haven't gotten enough of Salsa20 at this point, as one
last hurrah you could rework Salsa20Encrypt for bits rather than
bytes. Cryptol makes this pretty easy to do -- in fact, if you've
written your Salsa20Encrypt function in a straightforward manner, all
you'd need to do is change the type signature and add one more `join`
in the definition. Actually, why don't you try it?

**EXERCISE**: Create a new function called Salsa20EncryptBits that works
just like Salsa20Encrypt except that it acts on a message which is
some number of bits `b` (such that `b /^ 8 <= 2^^70`), rather than `l`
bytes.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Cryptol Course ](../../README.md) ||
| [ < CRC ](../CRC/CRC.md) | **Salsa20** | [ Cryptographic Properties > ](../CryptoProofs/CryptoProofs.md) |
|| [ ? Salsa20 ](./Salsa20.md) ||

