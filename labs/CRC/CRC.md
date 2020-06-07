
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
polynomial G of degree-n. Cryptol has some support for polynomials,
for instance, one can express a polynomial like so `<| x^^3 + x + 1 |>
which is simply the four bit string `0b1011`. It's important to note
that even though this is a degree-3 polynomial, it takes four bits to
represent. So, in Cryptol, a degree-n+1 polynomial takes n bits. We'll
also need a message M which is simply a sequence of, say, m bits.

In the definition from [1], the message is extended (concatenated)
with n zeroes. Let's make our CRC a little more generic and support
extension with n user supplied bits. This new parameter broadens the
families of CRCs we can support. Let's call this parameter `fill`
because it's identical to the initial fill of a CRC when implemented
by a linear feedback shift register.

Cryptol supports multiplying (`pmul`), dividing (`pdiv`), and
performing the modulus (`pmod`) of polynomials. This is more than we
need to define a parameterized CRC function.

Our CRC definition,

```
CRC : {n, m} (fin n, fin m, n >= 1, m >= 1) => [n+1] -> [n] -> [m] -> [n]
CRC G fill M = pmod ((fill#0) ^ (M # (0 : [n]))) G

```

```
message = 0b11010011101100
CRCExample = CRC <| x^^3 + x + 1 |> 0

CRC32Posix M = (CRC <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |> 0 M) ^ 0xffffffff

CRC32BZIP2 M = (CRC <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |> 0xffffffff M) ^ 0xffffffff

CRC32 M = reverse (CRC <| x^^32 + x^^26 + x^^23 + x^^22 + x^^16 + x^^12 + x^^11 + x^^10 + x^^8 + x^^7 + x^^5 + x^^4 + x^^2 + x + 1 |> 0xffffffff (join (map reverse (groupBy`{8} (0#M))))) ^ 0xffffffff
```