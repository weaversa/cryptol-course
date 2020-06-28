# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```shell
cryptol> :m labs::CRC::CRC
```

We start by defining a new module for this lab:

```
module labs::CRC::CRC where
```

# Cyclic Redundancy Checks

From [1],

> A cyclic redundancy check (CRC) is an error-detecting code commonly
> used in digital networks and storage devices to detect accidental
> changes to raw data.

CRCs are important to cryptography because they are often used, in
part, to protect the integrity of key material (see Section 6.1 of
NIST's [Recommendation for Key
Management](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-57pt1r5.pdf).

In this lab we will create Cryptol specifications for a family of
different CRCs.


## Mathematics of CRCs

A simple mathematical definition of CRCs can be found here:
[Mathematics of cyclic redundancy
checks](https://en.wikipedia.org/wiki/Mathematics_of_cyclic_redundancy_checks#Maths)

> R(x) = M(x) * x<sup>n</sup> mod G(x)
>
> Here M(x) is the original message polynomial and G(x) is the
degree-n generator polynomial. The bits of M(x) * x<sup>n</sup> are
the original message with n zeroes added at the end. The CRC
'checksum' is formed by the coefficients of the remainder polynomial
R(x) whose degree is strictly less than n.

To start, our definition of CRC in Cryptol will need a generator
polynomial `G` of degree `n`. Cryptol has some support for
polynomials, for instance, one can express a polynomial like so `<|
x^^3 + x + 1 |>` which is simply the four bit string `0b1011`. It's
important to note that even though this is a degree three polynomial,
it takes four bits to represent. Generally, Cryptol's representation
of a degree `n` polynomial is a sequence of `n+1` bits where each
monomial is represented by a `True` bit in the sequence.  We'll also
need a message `M` which is simply a sequence of `m` bits. Notice that
the definition from [2] tells us that `M` is extended (concatenated)
with `n` zeroes prior to calculating the modulus.

Cryptol supports multiplying (`pmul`), dividing (`pdiv`), and
performing the modulus (`pmod`) of polynomials. This is more than we
need to define a simple CRC function.

**EXERCISE**: Here we provide a skeleton CRC for you to fill in
according to the definition above. Use the `CRCSimpleTest` property to
help you get it right.

```
CRCSimple :
    {n, m}
    (fin n, fin m) =>
    [n+1] -> [m] -> [n]
CRCSimple G M = undefined
  where M' = undefined  //Concatenate M with n zero bits
```

This test-case is from [1].

```
property CRCSimpleTest = CRCSimple G 0b11010011101100 == 0b100
  where G = <| x^^3 + x + 1 |>
```


## A Family of CRC32s

Bastian Molkenthin's [CRC
simulator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)
[3] supports nine different 32-bit CRCs.

| Name         | Polynomial  | Initial Fill    | Post-XOR   | Reflect Input Bytes | Reverse Output |
|--------------|-------------|-----------------|------------|---------------------|----------------|
| CRC32        | 0x104C11DB7 | 0xFFFFFFFF      | 0xFFFFFFFF | True                | True           |
| CRC32_BZIP2  | 0x104C11DB7 | 0xFFFFFFFF      | 0xFFFFFFFF | False               | False          |
| CRC32_C      | 0x11EDC6F41 | 0xFFFFFFFF      | 0xFFFFFFFF | True                | True           |
| CRC32_D      | 0x1A833982B | 0xFFFFFFFF      | 0xFFFFFFFF | True                | True           |
| CRC32_MPEG2  | 0x104C11DB7 | 0xFFFFFFFF      | 0x00000000 | False               | False          |
| CRC32_POSIX  | 0x104C11DB7 | 0x00000000      | 0xFFFFFFFF | False               | False          |
| CRC32_Q      | 0x1814141AB | 0x00000000      | 0x00000000 | False               | False          |
| CRC32_JAMCRC | 0x104C11DB7 | 0xFFFFFFFF      | 0x00000000 | True                | True           |
| CRC32_XFER   | 0x1000000AF | 0x00000000      | 0x00000000 | False               | False          |

Here we can see that the CRCs differ not only in polynomials (choice
of `G`) but also in four other parameters that we'll go over
soon. Though first, two of these, namely, `CRC32_Q` and `CRC32_XFER`,
are simple enough to be compatible with `CRCSimple`.

Here we define a test message using the [canonical
pangram](https://en.wikipedia.org/wiki/The_quick_brown_fox_jumps_over_the_lazy_dog).

```
testM = "The quick brown fox jumps over the lazy dog"
```

```
property CRCSimple_QTest = CRCSimple G (join testM) == 0xf4965ffc
  where G = <| x^^32 + x^^31 + x^^24 + x^^22 + x^^16 + x^^14 + x^^8 + x^^7 + x^^5 + x^^3 + x + 1 |>
```

```
property CRCSimple_XFERTest = CRCSimple G (join testM) == 0x140493e5
  where G = <| x^^32 + x^^7 + x^^5 + x^^3 + x^^2 + x + 1 |>
```

**EXERCISE**: Make sure these two properties prove using the
`CRCSimple` function you defined above.


### Fully Parameterized CRC

To support the full suite of CRC32's from [3], we need to add four
parameters.

* Initial Fill (`fill`)
   * Refers to the initial fill of a CRC when implemented by a linear
     feedback shift register. Since we're implementing CRC here with
     polynomial arithmetic, we can add this parameter by XORing the
     initial fill into the high-order bits of the zero-expanded
     message before calculating the modulus.
* Post-XOR (`post`)
    * A sequence of bits that are XOR'd into the modulus to create the
      final output.
* Reflect Input Bytes (`rib`)
    * Denotes whether or not the input (when viewed as a sequence of
      bytes) should have the bits inside each byte reversed.
* Reverse Output (`ro`)
    * Denotes whether or not the output (when viewed as a bitvector)
      should be reversed.

**EXERCISE**: Here we provide a skeleton for a fully parameterized CRC
for you to fill in. Please augment the `CRCSimple` function with the
four extra parameters given above. Use the `CRC32Test` property to
help you get it right.

Note, since there is now a parameter that possibly reflects the input
bytes, to make things a little easier here, we've reshaped the input
`M` as a sequence of bytes.

```
CRC :
    {n, m}
    (fin n, fin m) =>
    [n+1] -> [n] -> [n] -> Bit -> Bit -> [m][8] -> [n]
CRC G fill post rib ro M =
    undefined //if R should be reversed, then reverse R, else return R
  where
    R      = undefined //fill' XOR M'' modulus G XOR post
    M'     = undefined //reflect the input bytes, if necessary, and then join the bytes into a bitvector
    M''    = undefined //extend M' with n zero bits
    fill'  = undefined //extend fill with (m*8) zero bits so that fill' matches the type of M''
```

Here is a definition of CRC32, using the parameterized `CRC` function.

```
CRC32 = CRC G 0xffffffff 0xffffffff True True
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32Test =
    CRC32 testM == 0x414FA339
```

**EXERCISE**: Use the values in the table above to fill in values for
each of the following eight 32-bit CRCs. Use the associated properties
to help you get them right, or test them yourself with the [CRC
simulator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)
[3].


### BZIP2

```
CRC32_BZIP2 = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32_BZIP2Test =
    CRC32_BZIP2 testM == 0x459DEE61
```


### C

```
CRC32_C = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^28 + x^^27 + x^^26 + x^^25 + x^^23 + x^^22 + x^^20 + x^^19 + x^^18 + x^^14 + x^^13 + x^^11 + x^^10 + x^^9 + x^^8 + x^^6 + 1 |>

property CRC32_CTest =
    CRC32_C testM == 0x22620404

```


### D

```
CRC32_D = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^31 + x^^29 + x^^27 + x^^21 + x^^20 + x^^17 + x^^16 + x^^15 + x^^12 + x^^11 + x^^5 + x^^3 + x + 1 |>

property CRC32_DTest =
    CRC32_D testM == 0x9D251C62
```


### MPEG2

```
CRC32_MPEG2 = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32_MPEG2Test =
    CRC32_MPEG2 testM == 0xBA62119E
```


### POSIX

```
CRC32_POSIX = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC_POSIXTest =
    CRC32_POSIX testM == 0x36B78081
```


### Q

```
CRC32_Q = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^31 + x^^24 + x^^22 + x^^16 + x^^14 + x^^8 + x^^7 + x^^5 + x^^3 + x + 1 |>

property CRC32_QTest = 
    CRC32_Q testM == 0xF4965FFC
```


### JAMCRC

```
CRC32_JAMCRC = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32_JAMCRCTest =
    CRC32_JAMCRC testM == 0xBEB05CC6
```


### XFER

```
CRC32_XFER = CRC G undefined undefined undefined undefined
  where G = <| x^^32 + x^^7 + x^^5 + x^^3 + x^^2 + x + 1 |>

property CRC32_XFERTest =
    CRC32_XFER testM == 0x140493E5
```


## Parting Exercises

This lab defined the 32-bit CRCs from [3]. You might also
consider defining the 8, 16, and 64-bit CRC's from [3] or any of the
CRCs given in [1], Section "Polynomial representations of cyclic
redundancy checks".


# Bibliography

[1] Cyclic redundancy check. In Wikipedia. Retrieved June 7th, 2020,
from https://en.wikipedia.org/wiki/Cyclic_redundancy_check.

[2] Mathematics of cyclic redundancy checks. In Wikipedia. Retrieved June 7th, 2020, from https://en.wikipedia.org/wiki/Mathematics_of_cyclic_redundancy_checks

[3] CRC Calculator. Bastian Molkenthin. Retrieved June 7th, 2020, from http://www.sunshine2k.de/coding/javascript/crc/crc_js.html
