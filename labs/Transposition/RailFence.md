(WORK IN PROGRESS)

# Rail Fence

In the rail fence cipher, a message is placed one character at a time 
on "rails" in a zigzag pattern, e.g. (from the Wikipedia article).

```cryptol
// For 3 rails:
// W . . . E . . . C . . . R . . . L . . . T . . . E
// . E . R . D . S . O . E . E . F . E . A . O . C .
// . . A . . . I . . . V . . . D . . . E . . . N . .

/** test # additional rails */
test_r = 2

/** test plaintext */
test_msg = "WEAREDISCOVEREDFLEEATONCE"

/** test ciphertext */
test_msg' = "WECRLTEERDSOEEFEAOCAIVDEN"
```

Given this problem description, our transposition cipher will need to 
return a permutation based on the length of a message and the number 
of rails.

```cryptol
/** constraint on number of additional rails (excluding first rail) */
type constraint _r_ r = (fin r, r >= 1)
/** constraint on message length */
type constraint _m_ m = (fin m, m >= 1)

/** type of padded message index */
type pw m r = width (m /^ (2*r) * (2*r))

/** indices per cycle */
cycle : {r} _r_ r => (SeqIndex (2*r), [r-1][2](SeqIndex (2*r)), SeqIndex (2*r))
cycle = (0, transpose [take`{r-1} ([1...]:[inf](SeqIndex (2*r))), reverse (take`{r-1} ([`(r+1)...]:[inf](SeqIndex (2*r))))], `r)

/** condensed indices to encrypt message of length `m` over number of additional rails `r` */
pi : {r, m} (_r_ r, _m_ m) => Permutation m
pi = out'
  where
    (a,b,c) = cycle`{r}
    groups =
      [ ( zext a
        , [ map zext [l, h] | [l,h] <- b ]
        , zext c )
        + ( o
          , take (repeat [o,o])
          , o)
      | o <- map zext (take`{m /^ (2*r)} [0,`(2*r)...] : [m /^ (2*r)][pw m r]) ]
    idxs = groups.0 # join (join (transpose (groups.1))) # groups.2
    out = [(idxs, 0, 0)]
        # [ if w' @ i < (`m: [pw m r]) then (w', i+1, j)
             | j <= i                  then (w', i, i+1)
             | w' @ j < (`m: [pw m r]) then (swap w' i j, i+1, j+1)
            else (w', i, j+1)
          | (w', i, j) <- out
          | _ <- tail [0 .. (m /^ (2*r) * (2*r)) : [pw m r]] ]
    out' = map take`{width m} (take`{m} (last out).0)


/** inverse of condensed pi to decrypt message of length `m` over number of additional rails `r` */
pi': {r, m} (_r_ r, _m_ m) => Permutation m
pi' = inverse pi`{r, m}

// test values from [Wikipedia article on Rail Fence Cipher](https://en.wikipedia.org/wiki/Rail_fence_cipher)

/** Wikipedia test encryption passes */
property test = encrypt pi`{test_r} test_msg == test_msg'
/** Wikipedia test decryption passes */
property test' = decrypt pi`{test_r} test_msg' == test_msg

/**
 * Decryption recovers encrypted message `msg` of length `m` and 
 * element type `a` over `r` additional rails
 */
recovery_: {r, m, a} (_r_ r, _m_ m, Cmp a) => [m]a -> Bit
recovery_ msg = inverts (decrypt pi`{r}) (encrypt pi`{r})

property recovery_1_1 = recovery_`{1, 1, Char}
property recovery_1_2 = recovery_`{1, 2, Char}
property recovery_1_4 = recovery_`{1, 4, Char}
property recovery_1_8 = recovery_`{1, 8, Char}
property recovery_1_16 = recovery_`{1, 16, Char}
property recovery_1_32 = recovery_`{1, 32, Char}
property recovery_1_64 = recovery_`{1, 64, Char}
property recovery_1_128 = recovery_`{1, 128, Char}
property recovery_1_337 = recovery_`{1, 337, Char}

property recovery_2_1 = recovery_`{2, 1, Char}
property recovery_2_2 = recovery_`{2, 2, Char}
property recovery_2_4 = recovery_`{2, 4, Char}
property recovery_2_8 = recovery_`{2, 8, Char}
property recovery_2_16 = recovery_`{2, 16, Char}
property recovery_2_32 = recovery_`{2, 32, Char}
property recovery_2_64 = recovery_`{2, 64, Char}
property recovery_2_128 = recovery_`{2, 128, Char}

property recovery_3_1 = recovery_`{3, 1, Char}
property recovery_3_2 = recovery_`{3, 2, Char}
property recovery_3_4 = recovery_`{3, 4, Char}
property recovery_3_8 = recovery_`{3, 8, Char}
property recovery_3_16 = recovery_`{3, 16, Char}
property recovery_3_32 = recovery_`{3, 32, Char}
property recovery_3_64 = recovery_`{3, 64, Char}
property recovery_3_128 = recovery_`{3, 128, Char}

property recovery_4_1 = recovery_`{4, 1, Char}
property recovery_4_2 = recovery_`{4, 2, Char}
property recovery_4_4 = recovery_`{4, 4, Char}
property recovery_4_8 = recovery_`{4, 8, Char}
property recovery_4_16 = recovery_`{4, 16, Char}
property recovery_4_32 = recovery_`{4, 32, Char}
property recovery_4_64 = recovery_`{4, 64, Char}
property recovery_4_128 = recovery_`{4, 128, Char}

property recovery_8_128 = recovery_`{8, 128, Char}
property recovery_8_256 = recovery_`{8, 256, Char}
property recovery_8_1024 = recovery_`{8, 1024, Char}
property recovery_8_4096 = recovery_`{8, 4096, Char}

property recovery_13_37 =  recovery_`{13, 37, Char}
property recovery_13_128 = recovery_`{13, 128, Char}
property recovery_13_256 = recovery_`{13, 256, Char}
property recovery_13_1024 = recovery_`{13, 1024, Char}
property recovery_13_4096 = recovery_`{13, 4096, Char}

property recovery_16_128 = recovery_`{16, 128, Char}
property recovery_16_256 = recovery_`{16, 256, Char}
property recovery_16_1024 = recovery_`{16, 1024, Char}
property recovery_16_4096 = recovery_`{16, 4096, Char}

```

# Conclusion

This lab presented the Rail Fence transposition cipher, which is 
deceptively challenging to specify in Cryptol, but we did it!  
Completing this and prior labs should provide a strong foundation 
from which to specify more realistic transposition ciphers.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

Up: [Course README](README.md)
Previous: [Scytale: A classic easy-to-specify transposition cipher](Scytale.md)
Next: [We've worked hard enough -- your turn!](Route.md)
