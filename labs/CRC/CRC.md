```
module labs::CRC::CRC where
```

[Mathematics of cyclic redundancy
checks](https://en.wikipedia.org/wiki/Mathematics_of_cyclic_redundancy_checks#Maths)

From [1],

> ![](https://render.githubusercontent.com/render/math?math=R(x)%20=%20M(x)%20\cdot%20x^n%20\,\bmod\,%20G(x))
Here M(x) is the original message polynomial and G(x) is the degree-n
generator polynomial. The bits of M(x) * x^^n are the original message
with n zeroes added at the end. The CRC 'checksum' is formed by the
coefficients of the remainder polynomial R(x) whose degree is strictly
less than n.

To start, our definition of CRC in Cryptol will need a generator
polynomial `G` of degree `n`. Cryptol has some support for polynomials,
for instance, one can express a polynomial like so `<| x^^3 + x + 1 |>`
which is simply the four bit string `0b1011`. It's important to note
that even though this is a degree three polynomial, it takes four bits
to represent. Generally, Cryptol's representation of a degree `n+1`
polynomial is a sequence of `n` bits where each monomial is
represented by a `True` in the sequence.  We'll also need a message
`M` which is simply a sequence of, say, `m` bits. In the definition
from [1], `M` is extended (concatenated) with `n` zeroes.

Cryptol supports multiplying (`pmul`), dividing (`pdiv`), and
performing the modulus (`pmod`) of polynomials. This is more than we
need to define a simple CRC function.

```
CRCSimple :
    {n, m}
    (fin n, fin m) =>
    [n+1] -> [m] -> [n]
CRCSimple G M = pmod M' G
  where M' = M # 0 : [m+n]
```

This testcase is from [1].

```
property CRCSimpleTest =
    CRCSimple G 0b11010011101100 == 0b100
  where
    G  = <| x^^3 + x + 1 |>
```

Bastian Molkenthin's [CRC
simulator](http://www.sunshine2k.de/coding/javascript/crc/crc_js.html)
[2] lists nine different 32-bit CRCs.

| Name         | Polynomial  | Initial Fill    | Post-XOR   | Input Bytes Reflected | Output Reversed |
|--------------|-------------|-----------------|------------|-----------------------|-----------------|
| CRC32        | 0x104C11DB7 | 0xFFFFFFFF      | 0xFFFFFFFF | True                  | True            |
| CRC32_BZIP2  | 0x104C11DB7 | 0xFFFFFFFF      | 0xFFFFFFFF | False                 | False           |
| CRC32_C      | 0x11EDC6F41 | 0xFFFFFFFF      | 0xFFFFFFFF | True                  | True            |
| CRC32_D      | 0x1A833982B | 0xFFFFFFFF      | 0xFFFFFFFF | True                  | True            |
| CRC32_MPEG2  | 0x104C11DB7 | 0xFFFFFFFF      | 0x00000000 | False                 | False           |
| CRC32_POSIX  | 0x104C11DB7 | 0x00000000      | 0xFFFFFFFF | False                 | False           |
| CRC32_Q      | 0x1814141AB | 0x00000000      | 0x00000000 | False                 | False           |
| CRC32_JAMCRC | 0x104C11DB7 | 0xFFFFFFFF      | 0x00000000 | True                  | True            |
| CRC32_XFER   | 0x1000000AF | 0x00000000      | 0x00000000 | False                 | False           |

Here we can see that the polynomials differ not only in polynomials
(choice of `G`) but also in four other parameters that we'll go over
soon. Though first, two of these, namely, `CRC32_Q` and `CRC32_XFER`,
are simple enough to be compatible with `CRCSimple`.

Here we define a test `M` using the [canonical
pangram](https://en.wikipedia.org/wiki/The_quick_brown_fox_jumps_over_the_lazy_dog)

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

## Parameterized CRC

To support the full suite of CRC32's from [2], we need to add four parameters.

* Initial Fill
* Post-XOR
* Input Bytes Reflected
* Output Reversed

The "Initial Fill" parameter refers to the initial fill of a CRC when
implemented by a linear feedback shift register. Since we're
implementing CRC here with polynomial arithmetic, we can add this
parameter by XORing the initial fill into the high-order bits of the
zero-expanded message before calculating the modulus.

The "Post-XOR" parameter is a sequence of bits that are XOR'd into the
modulus to create the final output.

The "Input Bytes Reflected" parameter denotes whether or not the input
(when viewed as a sequence of bytes) should have the bits inside each
byte reversed.

The "Output Reversed" parameter denotes whether or not the output
(when viewed as a bitvector) should be reversed.



The fully parameterized CRC definition,

```
CRC :
    {n, m}
    (fin n, fin m) =>
    [n+1] -> [n] -> [n] -> Bit -> Bit -> [m][8] -> [n]
CRC G fill post rib ro M =
    if ro then reverse R else R
  where
    R      = pmod (fill' ^ M'') G ^ post
    M'     = join (if rib then (map reverse M) else M)
    M''    = M'   # (0 : [n])
    fill'  = fill # (0 : [m*8])

```

```
CRC32 = CRC G 0xffffffff 0xffffffff True True
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32Test =
    CRC32 testM == 0x414FA339
```
  
```
CRC32_BZIP2 = CRC G 0xffffffff 0xffffffff False False
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32_BZIP2Test =
    CRC32_BZIP2 testM == 0x459DEE61

```

```
CRC32_C = CRC G 0xffffffff 0xffffffff True True
  where G = <| x^^32 + x^^28 + x^^27 + x^^26 + x^^25 + x^^23 + x^^22 + x^^20 + x^^19 + x^^18 + x^^14 + x^^13 + x^^11 + x^^10 + x^^9 + x^^8 + x^^6 + 1 |>

property CRC32_CTest =
    CRC32_C testM == 0x22620404

```

```
CRC32_D = CRC G 0xffffffff 0xffffffff True True
  where G = <| x^^32 + x^^31 + x^^29 + x^^27 + x^^21 + x^^20 + x^^17 + x^^16 + x^^15 + x^^12 + x^^11 + x^^5 + x^^3 + x + 1 |>

property CRC32_DTest =
    CRC32_D testM == 0x9D251C62
```

```
CRC32_MPEG2 = CRC G 0xffffffff 0x00000000 False False
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32_MPEG2Test =
    CRC32_MPEG2 testM == 0xBA62119E
```

```
CRC32_POSIX = CRC G 0x00000000 0xffffffff False False
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC_POSIXTest =
    CRC32_POSIX testM == 0x36B78081
```

```
CRC32_Q = CRC G 0x00000000 0x00000000 False False
  where G = <| x^^32 + x^^31 + x^^24 + x^^22 + x^^16 + x^^14 + x^^8 + x^^7 + x^^5 + x^^3 + x + 1 |>

property CRC32_QTest = 
    CRC32_Q testM == 0xF4965FFC
```

```
CRC32_JAMCRC = CRC G 0xffffffff 0x00000000 True True
  where G = <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |>

property CRC32_JAMCRCTest =
    CRC32_JAMCRC testM == 0xBEB05CC6
```

```
CRC32_XFER = CRC G 0x00000000 0x00000000 False False
  where G = <| x^^32 + x^^7 + x^^5 + x^^3 + x^^2 + x + 1 |>

property CRC32_XFERTest =
    CRC32_XFER testM == 0x140493E5
```
