# Salsa20

Salsa20 is a hash function created by Daniel J. Bernstein. The
specification is available [here](https://cr.yp.to/snuffle/spec.pdf),
but also provided in this repository
[here](/weaversa/cryptol-course/labs/Salsa20/Salsa20_spec.pdf) for
ease of access. The specification document describes Salsa20 as well
as how to use it as a [stream
cipher](https://en.wikipedia.org/wiki/Stream_cipher) in [counter
mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Counter_(CTR)).

This lab will go through the Salsa20 specification document section by
section, showing how to write a fairly pedantic Cryptol specification
of Salsa20. But first, since we are creating a module, the first line
needs to be the module definition.

```
module labs::Salsa20::Salsa20 where
```

Now we can begin to dig into the specification document!

## 1 Introduction

The last paragraph of the introduction defines a **byte** to mean an
element of {0,1,...,255} and says that

> there are many common ways to represent a byte.

This is an opportunity for us to specify a representation in our
specification. Here, we define `Bytes` as a new type synonym for a
sequence of `n` 8-bit words.

```
type Bytes n = [n][8]
```


## 2 Words

This section defines **word** to be an element of {0,1,...,2^32 -
1}. Similarly to `Bytes` above, we define `Words` as a new type
synonym for a sequence of `n` 32-bit words.

```
type Words n = [n][32]
```

The specification then goes on to give an example of how words are
expressed in hexadecimal throughout the rest of the document. Here, we
show that Cryptol natively agrees with this expression by encoding the
example as a property.

```
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

This document is interactive and literate. So, takes some time now to
open this document in the Cryptol interpreter.

```bash
┏━╸┏━┓╻ ╻┏━┓╺┳╸┏━┓╻  
┃  ┣┳┛┗┳┛┣━┛ ┃ ┃ ┃┃  
┗━╸╹┗╸ ╹ ╹   ╹ ┗━┛┗━╸
version 2.8.1 (e914cef)
https://cryptol.net  :? for help

Loading module Cryptol
Cryptol> :m labs::Salsa20::Salsa20
Loading module labs::Salsa20::Salsa20
labs::Salsa20::Salsa20>
```

Now that the document is open, let's prove `hexadecimalProp`:

```bash
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

```
property sumProp =
    0xc0a8787e + 0x9fd1161d == 0x60798e9b
```

Next, **exclusive-or** and **c-bit left rotation** are defined and
examples provided. One small syntactic change is needed, namely, the
exclusive-or symbol in Cryptol is `^` rather than ⊕. So, in Cryptol,
these examples become:

```
property exclusiveOrProp =
    0xc0a8787e ^ 0x9fd1161d == 0x5f796e63

property LeftRotationProp =
    0xc0a8787e <<< 5 == 0x150f0fd8
```


## 3 The quarterround Function


### Inputs and outputs

This section begins by giving the type of the quarterround function as:

> If y is a 4-word sequence then quarterround(y) is a 4-word sequence.

In Cryptol, we would write this type as:

```
quarterround : Words 4 -> Words 4
```


### Definition

This section then goes on to define quarterround. Two syntactic change
is required, namely, sequences in Cryptol are bookended by `[]` rather
than `()` and commas are not needed after statements in a where
clause.

```
quarterround [y0, y1, y2, y3] = [z0, z1, z2, z3] where
    z1 = y1 ^ ((y0 + y3) <<< 7)
    z2 = y2 ^ ((z1 + y0) <<< 9)
    z3 = y3 ^ ((z2 + z1) <<< 13)
    z0 = y0 ^ ((z3 + z2) <<< 18)
```

### Examples

Example input-ouput pairs are provided. Cryptol can make use of these
to provide evidence that quarterround was implemented correctly. Here
we create a property that will verify whether quarterround works
correctly on the provided example pairs.

```
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

```bash
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
   input (no ouput is missing).
   
In mathematics, this type of function would be called
bijective. Bijective functions are both injective and surjective
(one-to-one and onto). So, to prove that quarterround is invertible we
can prove that it is bijective. This can be done by showing that
quarterround is both injective and surjective. Now, since the domain
and range of quarterround are the same (`Word 4 -> Word 4`), we know
that, if the function is injective, it must also be surjective. Hence,
we only need to prove that quarterround is injective.

[Wikipedia
describes](https://en.wikipedia.org/wiki/Bijection,_injection_and_surjection)
defines an injective function as:

![](https://render.githubusercontent.com/render/math?math=\forall%20x,x'%20\in%20X,%20x%20\neq%20x'%20\Rightarrow%20f(x)%20\neq%20f(x'))

For the non-mathematician, this says that a function is injective if
every pair of different inputs causes the function to produce
different outputs. We can now encode this (almost verbatim) into a
Cryptol property.

```
property quarterroundIsInvertibleProp x x' =
    x != x' ==> quarterround x != quarterround x'
```

And then prove that the property is true.

```bash
labs::Salsa20::Salsa20> :prove quarterroundIsInvertibleProp 
Q.E.D.
(Total Elapsed Time: 0.430s, using "Z3")
```

It's worth noting here that Cryptol and associated theorem provers are
doing some very heavy lifting behind the scenes. Without an automated
theorem prover, the best one could do is run some tests. Cryptol does
support automated testing with it's `:check` command.

```bash
labs::Salsa20::Salsa20> :check quarterroundIsInvertibleProp 
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

```bash
labs::Salsa20::Salsa20> 2^^256 : Integer
115792089237316195423570985008687907853269984665640564039457584007913129639936
labs::Salsa20::Salsa20> :exhaust quarterroundIsInvertibleProp 
Using exhaustive testing.
Testing...     0%
```

While you wait, why not consider just how great automated theorem
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

```
rowround : Words 16 -> Words 16
```


### Definition

Exercise: Here we provide a skeleton for `rowround`. Please replacing
the `zero` symbols with the appropriate logic as given in the Salsa20
specification. You'll know you've gotten it right when it looks like
the specification and when `:prove rowroundExamplesProp` gives
`Q.E.D`.

```
rowround [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15] =
    [z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15]
  where
    [ z0,  z1,  z2,  z3] = quarterround [ y0,  y1,  y2,  y3]
    [ z5,  z6,  z7,  z4] = quarterround [ y5,  y6,  y7,  y4]
    [z10, z11,  z8,  z9] = quarterround [y10, y11,  y8,  y9]
    [z15, z12, z13, z14] = quarterround [y15, y12, y13, y14]
```


### Examples

```
property rowroundExamplesProp =
  (rowround [0x00000001, 0x00000000, 0x00000000, 0x00000000,
             0x00000001, 0x00000000, 0x00000000, 0x00000000,
             0x00000001, 0x00000000, 0x00000000, 0x00000000,
             0x00000001, 0x00000000, 0x00000000, 0x00000000] ==
            [0x08008145, 0x00000080, 0x00010200, 0x20500000,
             0x20100001, 0x00048044, 0x00000080, 0x00010000,
             0x00000001, 0x00002000, 0x80040000, 0x00000000,
             0x00000001, 0x00000200, 0x00402000, 0x88000100]) /\
  (rowround [0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
             0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
             0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
             0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a] ==
            [0xa890d39d, 0x65d71596, 0xe9487daa, 0xc8ca6a86,
             0x949d2192, 0x764b7754, 0xe408d9b9, 0x7a41b4d1,
             0x3402e183, 0x3c3af432, 0x50669f96, 0xd89ef0a8,
             0x0040ede5, 0xb545fbce, 0xd257ed4f, 0x1818882d])
```


### Comments

The comments in this section hint at an optimized way to perform
rowround. Here is one such optimized function:

```
rowroundOpt : Words 16 -> Words 16
rowroundOpt ys =
    join [ quarterround (yi <<< i) >>> i
         | yi <- split ys
	 | (i : [2])  <- [0 .. 3] ]
```

Exercise: Here we want to prove that for all inputs, rowroundOpt is
equal to rowround. Please replace the `zero` symbol below with such a
statement and then prove the property in the interpreter.

```
property rowroundOpProp ys = rowroundOpt ys == rowround ys
```

## 4 The columnround function


### Inputs and outputs

```
columnround : Words 16 -> Words 16
```

### Definition

Exercise: Here we provide a skeleton for `columnround`. Please replacing
the `zero` symbols with the appropriate logic as given in the Salsa20
specification. You'll know you've gotten it right when it looks like
the specification and when `:prove columnroundExamplesProp` gives
`Q.E.D`.


```
columnround [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15] =
    [y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15]
  where
    [ y0,  y4,  y8, y12] = quarterround [ x0,  x4,  x8, x12]
    [ y5,  y9, y13,  y1] = quarterround [ x5,  x9, x13,  x1]
    [y10, y14,  y2,  y6] = quarterround [x10, x14,  x2,  x6]
    [y15,  y3,  y7, y11] = quarterround [x15,  x3,  x7, x11]
```


### Examples

```
property columnroundExamplesProp =
  (columnround [0x00000001, 0x00000000, 0x00000000, 0x00000000,
                0x00000001, 0x00000000, 0x00000000, 0x00000000,
                0x00000001, 0x00000000, 0x00000000, 0x00000000,
                0x00000001, 0x00000000, 0x00000000, 0x00000000] ==
               [0x10090288, 0x00000000, 0x00000000, 0x00000000,
                0x00000101, 0x00000000, 0x00000000, 0x00000000,
                0x00020401, 0x00000000, 0x00000000, 0x00000000,
                0x40a04001, 0x00000000, 0x00000000, 0x00000000]) /\
  (columnround [0x08521bd6, 0x1fe88837, 0xbb2aa576, 0x3aa26365,
                0xc54c6a5b, 0x2fc74c2f, 0x6dd39cc3, 0xda0a64f6,
                0x90a2f23d, 0x067f95a6, 0x06b35f61, 0x41e4732e,
                0xe859c100, 0xea4d84b7, 0x0f619bff, 0xbc6e965a] ==
               [0x8c9d190a, 0xce8e4c90, 0x1ef8e9d3, 0x1326a71a,
                0x90a20123, 0xead3c4f3, 0x63a091a0, 0xf0708d69,
                0x789b010c, 0xd195a681, 0xeb7d5504, 0xa774135c,
                0x481c2027, 0x53a8e4b5, 0x4c1f89c5, 0x3f78c9c8])
```


### Comments

The comments in this section say that columnround is:

> ... simply the transpose of the rowround function



```
property columnround_is_transpose_of_rowround ys =
    rejigger (rowround ys) == columnround (rejigger ys)
  where
    rejigger a = join (transpose (split`{4} a))
```

```
doubleround : Words 16 -> Words 16
doubleround xs = rowround (columnround xs)
```

```
property doubleround_passes_tests =
  (doubleround [0x00000001, 0x00000000, 0x00000000, 0x00000000,
                0x00000000, 0x00000000, 0x00000000, 0x00000000,
                0x00000000, 0x00000000, 0x00000000, 0x00000000,
                0x00000000, 0x00000000, 0x00000000, 0x00000000] ==
               [0x8186a22d, 0x0040a284, 0x82479210, 0x06929051,
                0x08000090, 0x02402200, 0x00004000, 0x00800000,
                0x00010200, 0x20400000, 0x08008104, 0x00000000,
                0x20500000, 0xa0000040, 0x0008180a, 0x612a8020]) /\
  (doubleround [0xde501066, 0x6f9eb8f7, 0xe4fbbd9b, 0x454e3f57,
                0xb75540d3, 0x43e93a4c, 0x3a6f2aa0, 0x726d6b36,
                0x9243f484, 0x9145d1e8, 0x4fa9d247, 0xdc8dee11,
                0x054bf545, 0x254dd653, 0xd9421b6d, 0x67b276c1] ==
               [0xccaaf672, 0x23d960f7, 0x9153e63a, 0xcd9a60d0,
                0x50440492, 0xf07cad19, 0xae344aa0, 0xdf4cfdfc,
                0xca531c29, 0x8e7943db, 0xac1680cd, 0xd503ca00,
                0xa74b2ad6, 0xbc331c5c, 0x1dda24c7, 0xee928277])
```

```
littleendian : {a} (fin a) => Bytes a -> [a*8]
littleendian b = join (reverse b)
```

```
property littleendian_passes_tests =
  (littleendian [  0,   0,   0,   0] == 0x00000000) /\
  (littleendian [ 86,  75,  30,   9] == 0x091e4b56) /\
  (littleendian [255, 255, 255, 250] == 0xfaffffff)
```

```
littleendian' : {a} (fin a) => [a*8] -> Bytes a
littleendian' b = reverse (split b)
```

```
property littleendian_is_invertable_4 b = littleendian' (littleendian`{4} b) == b
property littleendian_is_invertable_8 b = littleendian' (littleendian`{8} b) == b
```

```
Salsa20 : Bytes 64 -> Bytes 64
Salsa20 xs = join ar
  where
    ar = [ littleendian' words | words <- xw + zs@10 ]
    xw = [ littleendian xi | xi <- split xs ]
    zs = [ xw ] # [ doubleround zi | zi <- zs ]
```

```
property Salsa20_passes_tests =
  (Salsa20 [  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
              0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
              0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
              0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0] ==
           [  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
              0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
              0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
              0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0]) /\
  (Salsa20 [211, 159,  13, 115,  76,  55,  82, 183,   3, 117, 222,  37, 191, 187, 234, 136,
             49, 237, 179,  48,   1, 106, 178, 219, 175, 199, 166,  48,  86,  16, 179, 207,
             31, 240,  32,  63,  15,  83,  93, 161, 116, 147,  48, 113, 238,  55, 204,  36,
             79, 201, 235,  79,   3,  81, 156,  47, 203,  26, 244, 243,  88, 118, 104,  54] ==
           [109,  42, 178, 168, 156, 240, 248, 238, 168, 196, 190, 203,  26, 110, 170, 154,
             29,  29, 150,  26, 150,  30, 235, 249, 190, 163, 251,  48,  69, 144,  51,  57,
            118,  40, 152, 157, 180,  57,  27,  94, 107,  42, 236,  35,  27, 111, 114, 114,
            219, 236, 232, 135, 111, 155, 110,  18,  24, 232,  95, 158, 179,  19,  48, 202]) /\
  (Salsa20 [ 88, 118, 104,  54,  79, 201, 235,  79,   3,  81, 156,  47, 203,  26, 244, 243,
            191, 187, 234, 136, 211, 159,  13, 115,  76,  55,  82, 183,   3, 117, 222,  37,
             86,  16, 179, 207,  49, 237, 179,  48,   1, 106, 178, 219, 175, 199, 166,  48,
            238,  55, 204,  36,  31, 240,  32,  63,  15,  83,  93, 161, 116, 147,  48, 113] ==
           [179,  19,  48, 202, 219, 236, 232, 135, 111, 155, 110,  18,  24, 232,  95, 158,
             26, 110, 170, 154, 109,  42, 178, 168, 156, 240, 248, 238, 168, 196, 190, 203,
             69, 144,  51,  57,  29,  29, 150,  26, 150,  30, 235, 249, 190, 163, 251,  48,
             27, 111, 114, 114, 118,  40, 152, 157, 180,  57,  27,  94, 107,  42, 236,  35])
```


```
Salsa20_expansion : {a} (a >= 1, 2 >= a) => Bytes (16*a) -> Bytes 16 -> Bytes 64
Salsa20_expansion k n = z
  where
    [s0, s1, s2, s3] = split "expand 32-byte k"
    [t0, t1, t2, t3] = split "expand 16-byte k"
    x = if (`a : [2]) == 1
        then t0 # k0 # t1 # n # t2 # k0 # t3
        else s0 # k0 # s1 # n # s2 # k1 # s3
    z = Salsa20 x
    [k0, k1] = split (k # zero)
```


```
property Salsa20_expansion_passes_tests =
    (Salsa20_expansion (k0#k1) n ==
     [  69,  37,  68,  39,  41,  15, 107, 193, 255, 139, 122,   6, 170, 233, 217,  98,
        89, 144, 182, 106,  21,  51, 200,  65, 239,  49, 222,  34, 215, 114,  40, 126,
       104, 197,   7, 225, 197, 153,  31,   2, 102,  78,  76, 176,  84, 245, 246, 184,
       177, 160, 133, 130,   6,  72, 149, 119, 192, 195, 132, 236, 234, 103, 246,  74]) /\
    (Salsa20_expansion k0 n ==
     [  39, 173,  46, 248,  30, 200,  82,  17,  48,  67, 254, 239,  37,  18,  13, 247,
       241, 200,  61, 144,  10,  55,  50, 185,   6,  47, 246, 253, 143,  86, 187, 225,
       134,  85, 110, 246, 161, 163,  43, 235, 231,  94, 171,  51, 145, 214, 112,  29,
        14, 232,   5,  16, 151, 140, 183, 141, 171,   9, 122, 181, 104, 182, 177, 193])
  where
    k0 = [1 .. 16]
    k1 = [201 .. 216]
    n  = [101 .. 116]
```


```
Salsa20_encrypt : {a, l} (a >= 1, 2 >= a, l <= 2^^70) => Bytes (16*a) -> Bytes 8 -> Bytes l -> Bytes l
Salsa20_encrypt k v m = c
  where
    salsa = take (join [ Salsa20_expansion k (v # littleendian' i) | i <- [0, 1 ... ] ])
    c = m ^ salsa
```
