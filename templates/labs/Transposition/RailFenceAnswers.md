# Introduction

This module presents the [Rail Fence](https://en.wikipedia.org/wiki/Transposition_cipher#Rail_Fence_cipher)
transposition cipher, in which a message is placed one character at a 
time on "rails" in a zigzag pattern.  This cipher is very simple on 
paper, but is somewhat of a challenge to specify in Cryptol.  

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed and
  * this module to load successfully.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter and
  * the `:prove` and `:sat` commands
  * ...

For a background on Rail Fence, we recommend perusing the Wikipedia 
entries on [transposition ciphers](https://en.wikipedia.org/wiki/Transposition_cipher) 
in general and [Rail Fence](https://en.wikipedia.org/wiki/Transposition_cipher#Rail_Fence_cipher) 
in particular (including the [main article](https://en.wikipedia.org/wiki/Rail_fence_cipher)).

## Skills You'll Learn

Specifying Rail Fence will provide a strong foundation with Cryptol 
data structures and manipulation, upon which to specify more 
realistic transposition ciphers and other crypto algorithms.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the 
Cryptol interpreter. Load this module from within the Cryptol 
interpreter running in the `cryptol-course` directory with:

```Xcryptol-session
Cryptol> :m labs::Transposition::RailFenceAnswers
Loading module Cryptol
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofsAnswers
Loading module labs::Transposition::CommonPropertiesAnswers
Loading module labs::Transposition::TranspositionAnswers
Loading module labs::Transposition::RailFenceAnswers
```

We start by defining the module for this lab:

```cryptol
module labs::Transposition::RailFenceAnswers where
```

Additionally, we will import some common properties and transposition 
cipher components to this spec:

```cryptol
import labs::Transposition::TranspositionAnswers
```

# Rail Fence

## Example

The aforementioned Wikipedia article presents the example message 
"WEAREDISCOVEREDFLEEATONCE" for 3 rails.  This message is transposed 
by reading, row-by-row, "WECRLTEERDSOEEFEAOCAIVDEN".

```cryptol
// For 3 rails:
// W . . . E . . . C . . . R . . . L . . . T . . . E
// . E . R . D . S . O . E . E . F . E . A . O . C .
// . . A . . . I . . . V . . . D . . . E . . . N . .

/** test number of rails */
type test_r = 3

/** test plaintext */
test_msg = "WEAREDISCOVEREDFLEEATONCE"

/** test ciphertext */
test_msg' = "WECRLTEERDSOEEFEAOCAIVDEN"
```

Given this problem description, our transposition cipher will need to 
return a permutation based on the length of a message and the number 
of rails.  The main article goes on to describe "cycles", which form 
Rail Fence's block-like components of the cipher.

**EXERCISE**: Given a number of rails `type r`, return the indices 
occurring in the first cycle.  Check your definition using 
`cycle_test`.  Here's a starter:

``cycle`{r=1} == [0]``
<pre>
0 ...
</pre>

``cycle`{r=2} == [0,1]``
<pre>
0 0 ...
 1 1
</pre>

``cycle`{r=3} == [0,1,3,2]``
<pre>
0   0   0   ...
 1 3 1 3 1 3
  2   2   2
</pre>

``cycle`{r=4} == [0,1,5,2,4,3]``
<pre>
0     0     0     ...
 1   5 1   5 1   5
  2 4   2 4   2 4
   3     3     3
</pre>

(Hint: Your definition will most likely include logic distinguishing 
the top, bottom, and middle rails.  You might wish to implement a 
helper function that keeps this distinction intact for computation of 
`pi` in the next step.)

```cryptol
/** indices per cycle, tupled */
cycle':
    {r} (fin r, r >= 1) =>
    ([width (max 1 (2*(r - 1)))], [max 2 r - 2][2][width (max 1 (2*(r - 1)))], [width (max 1 (2*(r - 1)))])
cycle' = (0, m, `r - 1)
  where
    ml = take`{max 2 r - 2} [1...]
    mr = reverse (take`{max 2 r - 2} [`r...])
    m = transpose [ ml, mr ]

/** indices per cycle */
cycle:
    {r} (fin r, r >= 1) =>
    [max 1 (2*(r - 1))][width (max 1 (2*(r - 1)))]
cycle = take`{max 1 (2*(r - 1))} ([0] # join m # [`r - 1])
  where
    ml = take`{max 2 r - 2} [1...]
    mr = reverse (take`{max 2 r - 2} [`r...])
    m = transpose [ ml, mr ]

/** `cycle` is defined correctly for various numbers of rails */
property cycle_test = and
    [ cycle`{1} == [0]
    , cycle`{2} == [0,1]
    , cycle`{3} == [0,1,3,2]
    , cycle`{4} == [0,1,5,2,4,3]
    ]
```

```Xcryptol-session
labs::Transposition::RailFenceAnswers> :check cycle_test
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

Having broken down a Rail Fence message into cycles, we can try to 
define `pi` for an arbitrary message length.  All that remains is to 
increment subsequent indices by a multiple of the cycle length and 
process the resulting indices in the correct order, along each rail.  

**EXERCISE**: Bearing in mind that `+` is defined structurally (e.g. 
`(0,[[1,5],[2,4]],3) + (6,[[6,6],[6,6]],6)` is a valid expression), 
define `pi` and `pi'` to return a correct order for a Rail Fence 
encryption, again given the number of rails `type r` and sequence 
length `type n`.  (You'll have to add `r` as a type parameter.)  
Use `pi_test` to check your definition.

```cryptol
/** condensed indices to encrypt message of length `m` over number of additional rails `r` */
pi:
    {r, n}
    (fin r, r >= 1, fin n) =>
    [n][width n]
pi = out
  where
    (a,b,c) = cycle'`{r}
    groups =
      [ ( zext a, [map zext _b | _b <- b], zext c ) + ( o, take (repeat [o,o]), o)
      | o <- take`{max 1 (n /^ (max 1 (2*(r-1))))} ([0...] * repeat (`(max 1 (2*(r-1))))) ]
    idxs = groups.0 # join (join (transpose (groups.1))) # groups.2
    out = unpad`{n} idxs

/** inverse of condensed pi to decrypt message of length `m` over number of additional rails `r` */
pi': {r, n}
    (fin r, r >= 1, fin n) =>
    [n][width n]
pi' = inverse pi`{r, n}

/** tests from [Wikipedia article on Rail Fence Cipher](https://en.wikipedia.org/wiki/Rail_fence_cipher) pass */
property pi_test = and
    [ encrypt pi`{test_r} "" == ""
    , encrypt pi`{test_r} test_msg == test_msg'
    , encrypt pi`{test_r} "I_REALLY_LIKE_PUZZLES" == "IA_EZS_ELYLK_UZERLIPL"
    , encrypt pi`{1} "" == ""
    , encrypt pi`{1} test_msg == test_msg
    , encrypt pi`{1} "I_REALLY_LIKE_PUZZLES" == "I_REALLY_LIKE_PUZZLES"
    , decrypt pi`{test_r} "" == ""
    , decrypt pi`{test_r} test_msg' == test_msg
    , decrypt pi`{test_r} "IA_EZS_ELYLK_UZERLIPL" == "I_REALLY_LIKE_PUZZLES"
    , decrypt pi`{1} "" == ""
    , decrypt pi`{1} test_msg == test_msg
    , decrypt pi`{1} "I_REALLY_LIKE_PUZZLES" == "I_REALLY_LIKE_PUZZLES"
    ]
```

```Xcryptol-session
labs::Transposition::RailFenceAnswers> :check pi_test
Using exhaustive testing.
Passed 1 tests.
Q.E.D.
```

# Conclusion

This lab presented the Rail Fence transposition cipher, which is 
deceptively challenging to specify in Cryptol, but we did it!  
Completing this and prior labs should provide a strong foundation 
from which to specify more realistic transposition ciphers.

{{ solicitation }}

{{ navigation }}
