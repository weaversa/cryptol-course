
```
module labs::CRC::CRC where
```

[Mathematics of cyclic redundancy
checks](https://en.wikipedia.org/wiki/Mathematics_of_cyclic_redundancy_checks#Maths)

![](https://render.githubusercontent.com/render/math?math=R(x)%20=%20M(x)%20\cdot%20x^n%20\,\bmod\,%20G(x))

> Here M(x) is the original message polynomial and G(x) is the
degree-n generator polynomial.

> The bits of M(x) * x^^n are the original message with n zeroes added
at the end.

> The CRC 'checksum' is formed by the coefficients of the remainder
polynomial R(x) whose degree is strictly less than n.

```
CRC : {n, m} (fin n, fin m, n >= 1) => [n+1] -> [n] -> [m+1] -> [n]
CRC G fill M = pmod (pmult M fill) G
```