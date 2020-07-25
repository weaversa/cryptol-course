```cryptol
module labs::Demos::RailFence where

/** constraint on number of additional rails (excluding first rail) */
type constraint _r_ r = (fin r, r >= 1)
/** constraint on message length */
type constraint _m_ m = (fin m, m >= 1)

/** width of cycle indices */
type cw r = max 2 (width (2*r - 1))
/** width of message indices */
type pw m r = width (max m (max (2*r) (m /^ (2*r) * (2*r))))

/** Swap `i`th and `j`th entries of sequence `a` */
swap : {n, i, a} (fin i, i == width n + 1) => [n]a -> [i] -> [i] -> [n]a
swap a i j = updates a [i,j] (a @@ [j,i])

/** indices per cycle */
cycle: {r} _r_ r => ([cw r], [r-1][2][cw r], [cw r])
cycle = (0, transpose [take`{r-1} ([1...]:[inf][cw r]), reverse (take`{r-1} ([`(r+1)...]:[inf][cw r]))], `r)

/** inverse a 1:1 mapping of permuted values [0..`(n-1)] */
inverse_mapping: {n, w} (fin n, n >= 1, fin w, w >= width (n-1)) => [n][w] -> [n][w]
inverse_mapping seq = updates seq seq (take [0...])

/** condensed indices to encrypt message of length `m` over number of additional rails `r` */
indices: {r, m} (_r_ r, _m_ m) => [m][pw m r]
indices = take`{m} (last out).0
  where
    (a,b,c) = cycle`{r}
    groups = [ ( zext a 
               , [ map zext [l, h] | [l,h] <- b ]
               , zext c ) 
             + ( o
               , take (repeat [o,o])
               , o) 
             | o <- map zext (take`{m /^ (2*r)} [0,`(2*r)...] : [m /^ (2*r)][pw m r]) ]
    idxs = groups.0 # join (join (transpose (groups.1))) # groups.2
    out = [(idxs, 0, 0)]
        # [ if w'@i < `m then (w', i+1, j)
             | j <= i    then (w', i, i+1)
             | w'@j < `m then (swap w' i j, i+1, j+1)
            else (w', i, j+1)
          | (w', i, j) <- out
          | _ <- tail [0 .. (m /^ (2*r) * (2*r)) : [width (m /^ (2*r) * (2*r))]] ]


/** inverse of condensed indices to decrypt message of length `m` over number of additional rails `r` */
indices': {r, m} (_r_ r, _m_ m) => [m][pw m r]
indices' = inverse_mapping indices`{r, m}

/** encrypt msg via Rail Fence transposition cipher */
encrypt: {r, m, a} (_r_ r, _m_ m) => [m]a -> [m]a
encrypt msg = msg @@ indices`{r, m}

/** encrypt msg via Rail Fence transposition cipher */
decrypt: {r, m, a} (_r_ r, _m_ m) => [m]a -> [m]a
decrypt msg = msg @@ indices'`{r, m}

// test values from [Wikipedia article on Rail Fence Cipher](https://en.wikipedia.org/wiki/Rail_fence_cipher)
/** test number of additional rails */
type test_r = 2
/** test plaintext */
test_msg = "WEAREDISCOVEREDFLEEATONCE"
/** test ciphertext */
test_msg' = "WECRLTEERDSOEEFEAOCAIVDEN"

/** Wikipedia test encryption passes */
property test = encrypt`{test_r} test_msg == test_msg'
/** Wikipedia test decryption passes */
property test' = decrypt`{test_r} test_msg' == test_msg

/**
 * Decryption recovers encrypted message `msg` of length `m` and 
 * element type `a` over `r` additional rails
 */
recovery: {r, m, a} (_r_ r, _m_ m, Cmp a) => [m]a -> Bit
recovery msg = decrypt`{r} (encrypt`{r} msg) == msg

property recovery_1_1 = recovery`{1, 1, Char}
property recovery_1_2 = recovery`{1, 2, Char}
property recovery_1_4 = recovery`{1, 4, Char}
property recovery_1_8 = recovery`{1, 8, Char}
property recovery_1_16 = recovery`{1, 16, Char}
property recovery_1_32 = recovery`{1, 32, Char}
property recovery_1_64 = recovery`{1, 64, Char}
property recovery_1_128 = recovery`{1, 128, Char}
property recovery_1_337 = recovery`{1, 337, Char}

property recovery_2_1 = recovery`{2, 1, Char}
property recovery_2_2 = recovery`{2, 2, Char}
property recovery_2_4 = recovery`{2, 4, Char}
property recovery_2_8 = recovery`{2, 8, Char}
property recovery_2_16 = recovery`{2, 16, Char}
property recovery_2_32 = recovery`{2, 32, Char}
property recovery_2_64 = recovery`{2, 64, Char}
property recovery_2_128 = recovery`{2, 128, Char}

property recovery_3_1 = recovery`{3, 1, Char}
property recovery_3_2 = recovery`{3, 2, Char}
property recovery_3_4 = recovery`{3, 4, Char}
property recovery_3_8 = recovery`{3, 8, Char}
property recovery_3_16 = recovery`{3, 16, Char}
property recovery_3_32 = recovery`{3, 32, Char}
property recovery_3_64 = recovery`{3, 64, Char}
property recovery_3_128 = recovery`{3, 128, Char}

property recovery_4_1 = recovery`{4, 1, Char}
property recovery_4_2 = recovery`{4, 2, Char}
property recovery_4_4 = recovery`{4, 4, Char}
property recovery_4_8 = recovery`{4, 8, Char}
property recovery_4_16 = recovery`{4, 16, Char}
property recovery_4_32 = recovery`{4, 32, Char}
property recovery_4_64 = recovery`{4, 64, Char}
property recovery_4_128 = recovery`{4, 128, Char}

property recovery_8_128 = recovery`{8, 128, Char}
property recovery_8_256 = recovery`{8, 256, Char}
property recovery_8_1024 = recovery`{8, 1024, Char}
property recovery_8_4096 = recovery`{8, 4096, Char}

property recovery_13_37 =  recovery`{13, 37, Char}
property recovery_13_128 = recovery`{13, 128, Char}
property recovery_13_256 = recovery`{13, 256, Char}
property recovery_13_1024 = recovery`{13, 1024, Char}
property recovery_13_4096 = recovery`{13, 4096, Char}

property recovery_16_128 = recovery`{16, 128, Char}
property recovery_16_256 = recovery`{16, 256, Char}
property recovery_16_1024 = recovery`{16, 1024, Char}
property recovery_16_4096 = recovery`{16, 4096, Char}

```
