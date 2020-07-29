# Introduction

This library introduces and defines concepts common to transposition 
ciphers.

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed and
  * this module to load successfully.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter and
  * the `:prove` and `:sat` commands
  * ...

## Skills You'll Learn

This module will define a _transposition cipher_, in which message 
characters are _transposed_, in Cryptol using `@@` (to select items 
at multiple indices) and `updates` (to modify multiple items at 
selected indices with specified values).  Additionally, the module 
will recall some important cipher properties (that decryption 
recovers encrypted plaintext and that encryption is injective), where 
these operations are defined in terms of _permutations_.

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the 
Cryptol interpreter. Load this module from within the Cryptol 
interpreter running in the `cryptol-course` directory with:

```shell
Cryptol> :m labs::Transposition::Transposition
```

We start by defining the module for this lab:

```cryptol
module labs::Transposition::Transposition where
```

Additionally, we will import some common properties to apply to this 
spec:

```cryptol
import labs::Transposition::CommonProperties (injective, inverts)
```

# Transposition Ciphers

In a [_transposition cipher_](https://en.wikipedia.org/wiki/Transposition_cipher), 
messages are encrypted by _transposing_ (rearranging) characters, 
then decrypted by inverting this rearrangement to recover the 
original order.  Confidentiality depends upon a shared secret between 
a sender and receiver so that only they know how the message was 
transposed.  A _permutation mapping_, a 1:1 mapping from the ordered 
set of indices ``[0..`(m-1)]`` to a sequence of the same indices in a 
different order, formalizes this transposition. 

Before moving on to simple transposition ciphers such as Scytale, 
Rail Fence, and Route, we first go over some basics that will be 
required for transposition ciphers in general.

## Permutations and Mappings

A _mapping_ is an operation that associates an element of a given set 
(_domain_) with one or more elements of a (same or different) set 
(_range_).  A _bijective_ mapping maps each element of the domain to 
a distinct element of the range, and vice-versa.  A transposition 
cipher _transposes_ a message (a sequence of length `m`) to a 
scrambled message containing the same elements in a different order 
(a _permutation mapping_) known to the sender and receiver.  To 
decrypt a message, a receiver _inverts_ this permutation to yield the 
original message.

In Cryptol, we can use the `@@` operator to produce the transposition 
of a given message by a given permutation, and again to produce the 
original message by inverting that permutation (via a clever 
application of `updates`).  First, we define a sequence index:

```cryptol
/** type of index of sequence of width n */
type SeqIndex n = [width n]
```

(Note: It might be tempting to define this as `[width (n-1)]` as the 
weakest restriction on indices, but given the common need to compare 
indices with sequence length, it often becomes difficult to maintain 
specifications that satisfy Cryptol's type checker.  It's a lot 
easier to just use this definition...at least until the language 
developers improve sequence indexing, as planned for Cryptol 2.9...)

**EXERCISE**: Define a `PermutationMapping` type and a predicate 
`isPermutationMapping`, perhaps using the built-in functions `all` 
and `elem`, and the above `type SeqIndex`.  `isPermutationMapping pi` 
should return `True` iff a sequence `pi` comprises `n` elements, each 
of which is an index in the range `[0, n)`.

```cryptol
type PermutationMapping n = Zero n

isPermutationMapping : {n} fin n => PermutationMapping n -> Bit
isPermutationMapping pi = undefined
```

(Hint: ``take`{n} [0...]`` returns the first `n` numbers starting 
from `0`, i.e. the identity mapping.  `isPermutationMapping` can be 
defined using this sequence as one of the arguments to `all`.  It 
may be simpler to define a variant of `elem` that takes arguments in 
a different order.)

**EXERCISE**: Given a mapping `pi: PermutationMapping n`, use `updates` to 
return its inverse.  `:prove` your definition correct using 
`inverse_inverts`.

```cryptol
/** invert a `PermutationMapping` */
inverse : {n} fin n => PermutationMapping n -> PermutationMapping n
inverse pi = undefined

/** `inverse` inverts `PermutationMapping` `pi` */
inverse_inverts:
    {n, a} (fin n, Cmp a) =>
    PermutationMapping n -> [n]a -> Bit
property inverse_inverts pi msg =
    isPermutationMapping pi ==> msg @@ pi @@ (inverse pi) == msg
```

(Hint: The idiomatic solution to this exercise, where `inverse` is 
defined with ``take`{m} [0...]`` as one of the arguments to 
`updates`, is one of the most elegant in all of Cryptol.)

Alternatively, we can define the correctness of `inverse` in terms of 
a `permute` function (which applies a mapping `pi` to a message 
`msg`) and a generic function inverse property as described in 
`labs::CryptoProofs::CryptoProofs` and encapsulated in 
`labs::Transposition::CommonProperties`.

**EXERCISE**: Define `permute` to map indices `pi` of message `msg`, 
using `@@`.

```cryptol
permute : {n, a} fin n => PermutationMapping n -> [n]a -> [n]a
permute pi msg = undefined
```

**EXERCISE**: Define `permute'` such that `permute' pi` inverts 
`permute pi`, using `@@`, `pi`, and prior definitions.

```cryptol
permute' : {n, a} fin n => PermutationMapping n -> [n]a -> [n]a
permute' pi msg = undefined
```

**EXERCISE**: Define a `property` that `permute' pi` `inverts` 
`permute pi` if `pi` is a `PermutationMapping`, and `:prove` it for 
various message sizes and types.

```cryptol
/**
 * `permute pi'` inverts `permute pi` if `pi` is a 
 * `PermutationMapping`
 */
// permute'_inverts_permute:
//     {...} ... => ...
permute'_inverts_permute pi /* ... */ =
    undefined
```

**EXERCISE**: Define a `property` that `permute pi` is `injective` if 
`pi` is a `PermutationMapping`, and `:prove` it for various message 
sizes and types.

```cryptol
/** `permute pi` is `injective` if `pi` is a `PermutationMapping` */
// permute_injective:
//     {...} ... =>
//     ...
permute_injective pi /* ... */ = undefined
```

# Encryption and Decryption

With this foundation in place, we can define `encrypt` and `decrypt` 
operations for a transposition cipher.  All we need is a permutation 
mapping for a given message length.

**EXERCISE**: Define `encrypt` and `decrypt` in terms of `permute`, 
`permute'`, and a `PermutationMapping` `pi`.

```cryptol
/**
 * encrypt a message by transposition, given a function to generate a 
 * corresponding permutation mapping
 */
// encrypt:
//     {...} ... =>
//     ...
encrypt pi /* ... */ = undefined

/**
 * decrypt a message by transposition, given a function to generate a 
 * corresponding permutation mapping
 */
// decrypt:
//     {...} ... =>
//     ...
decrypt pi /* ... */ = undefined
```

**EXERCISE**: Define a `property` showing that `decrypt pf` `inverts` 
`encrypt pf` if `pf` returns a `PermutationMapping`, and `:prove` it 
for various message sizes and types.

```cryptol
/**
 * `decrypt pi` `inverts` `encrypt pi` 
 * if `pi` is a `PermutationMapping`
 */
// recovery:
//     ...
recovery pi /* ... */ = undefined
```

**EXERCISE**: Define a `property` showing that `encrypt pi` is 
`injective` if `pi` is a `PermutationMapping`, and `:prove` it 
for various message sizes and types.

```cryptol
/**
 * `encrypt pi` is `injective`
 * if `pi` is a `PermutationMapping`
 */
// encrypt_injective:
//     ...
encrypt_injective pi /* ... */ = undefined
```

# Padding and Filtering

Most transposition ciphers are based on an analogue to block size.  
For example, Scytale has a rod diameter, but not all messages wrap 
evenly around this rod, leaving a gap that must be managed when 
encrypting and decrypting an "uneven" message.  Likewise, in Rail 
Fence, messages are split by "cycles" determined by the number of 
rails in the fence, and not all messages are a multiple of cycle 
length.  One option to overcome such a limitation is to pad a 
message, encrypt it, send the encrypted padded message, and remove 
the padding characters after decryption.  However, since 
transposition ciphers are based on message _indices_ rather than 
_content_, the sender can simply derive a permutation mapping based 
on padded message length, remove indices that are out of place, and 
send the message permuted with this reduced mapping; the receiver can 
similarly derive the reduced permutation mapping, based on length, to 
decrypt the message.

Removing padding characters and reducing permutation mappings are 
examples of the abstract problem of sequence filtering:  Given a 
predicate `f: a -> Bit`, return from a sequence `seq: [n]a` the 
elements `seq': [m]a` such that `all f seq' == True`.

**EXERCISE**...just kidding. Normally, we might introduce an exercise 
to define `partition` at this point.  However, in Cryptol's type 
system, this turns out to be a somewhat difficult problem.  Indeed, 
this module's author, for whom all prior concepts covered in the lab 
came naturally, struggled for a day to arrive at what turned out to 
be an incorrect (but easily correctable) solution.  This problem 
baffled all but the lead instructor for the course.  Readers are 
invited to similarly struggle at this point, but hints showing a 
couple ways to solve this problem are provided below for those who 
instead wish to endure a diatribe on potential strategies to solve 
this problem.  Even we're not that mean!

## Index swapping

By definition, in a transposition cipher, characters of a message are 
rearranged.  The most basic rearrangement is to swap characters at 
two positions (indices) in a message (sequence).  However, while 
permutation mappings provide a more efficient mechanism to swap all 
characters at once, swapping turns out to be useful for the sequence 
filtering problem...

**EXERCISE**: Define a function of type `SwapFunction` below to swap 
items at indices `i` and `j` of a sequence `seq: [n]a` for number `n` 
and arbitrary character type `a`, using `@` and `update`, then again 
using `@@` and `updates` (do not use a temporary variable in either 
definition).  Use the `swap_equiv` property to verify that your 
definitions are equivalent, then the `swap_correct` property to show 
that one of them is correct.  (Because they are equivalent, this will 
infer that the other swap function is also correct.)

```cryptol
/** type of function that swaps items at index-pair */
type SwapFunction n a = [n]a -> SeqIndex n -> SeqIndex n -> [n]a
```

```cryptol
/** Swap `i`th and `j`th entries of sequence `a` via `@`/`update` */
swap_update : {n, a} fin n => SwapFunction n a
swap_update seq i j = update undefined

/** Swap `i`th and `j`th entries of sequence `a` via `@@`/`updates` */
swap_updates : {n, a} fin n => SwapFunction n a
swap_updates seq i j = updates undefined

// Define `swap` as either of above swap functions
swap = swap_updates
```

```cryptol
/** swap_update is functionally equivalent to swap_updates */
swap_equiv:
    {n, a}
    (fin n, Cmp a) =>
    [n]a -> SeqIndex n -> SeqIndex n -> Bit
property swap_equiv seq i j =
    swap_update seq i j == swap_updates seq i j

/** `swap` is correct; it just swaps values at specified indices */
swap_correct:
    {n, a}
    (fin n, Cmp a) =>
    [n]a -> SeqIndex n -> SeqIndex n -> SeqIndex n -> Bit
property swap_correct seq i j k =
    i < `n ==> j < `n ==> k < `n ==> 
    seq' @ k ==
        if k == i then seq @ j
        |  k == j then seq @ i
                  else seq @ k
      where
        seq' = swap seq i j
```

### Swap-Partitioning

Using `swap`, we can define a function that "partitions" a sequence 
into a subsequence of all elements in the original sequence that 
satisfy a predicate, followed by a subsequence of all elements that 
do not.  To achieve this, we can "walk" through the original 
sequence, and if the current character satisfies the predicate, keep 
walking; otherwise, swap the current character with the next one, and 
branch here while the subsequent character is another padding 
character.  For this strategy, the "current" sequence and index would 
be captured as a sequence comprehension (much as in a block cipher's 
iterations of a round function).

To better visualize this, suppose we've been given a String infused 
with padding characters `-`, and wish to move them to the end of the 
message.  The following function achieves this for any message length 
including the empty message `""` of length `0`:

```cryptol
/** Shift `-` characters to the end of a `String` */
rearrange: {n} (fin n) => String n -> String n
rearrange w = take (last out).0
  where
    out = [(w # [undefined], 0, 0)]
        # [ if w'@i != '-' then (w', i', j)
             | j <= i      then (w', i, i')
             | w'@j != '-' then (swap w' i j, i', j+1)
            else (w', i, j+1)
            where i' = zext`{width (n + 1)} i + 1
                  j' = zext`{width (n + 1)} j + 1
          | (w', i, j) <- out
          | _ <- tail [0 .. n : [width n]] ]

/** Visualize the steps of `rearrange` */
rearrange_trace: {n} (fin n) => String n -> [_](String (1+n), [width (1 + n)], [width (1 + n)])
rearrange_trace w = out
  where
    out = [(w # ['-'], 0, 0)]
        # [ if w'@i != '-' then (w', i', j)
             | j <= i      then (w', i, i')
             | w'@j != '-' then (swap w' i j, i', j+1)
            else (w', i, j+1)
            where i' = zext`{width (n + 1)} i + 1
                  j' = zext`{width (n + 1)} j + 1
          | (w', i, j) <- out
          | _ <- tail [0 .. n : [width n]] ]
```

Here's how this function works for the string `"HE-LL-O-"`:

```sh
labs::Transposition::Transposition> rearrange_trace "HE-LL-O-" 
[("HE-LL-O--", 0, 0), ("HE-LL-O--", 1, 0), ("HE-LL-O--", 2, 0),
 ("HE-LL-O--", 2, 3), ("HEL-L-O--", 3, 4), ("HELL--O--", 4, 5),
 ("HELL--O--", 4, 6), ("HELLO----", 5, 7), ("HELLO----", 5, 8)]
```

**EXERCISE**: Using `rearrange` as a blueprint, define a function 
`partition` that, given a predicate `f: a -> Bit` and sequence 
`seq: [n]a`, "partitions" the sequence, returning `seq': [n]a` 
such that there exists some `i: SeqIndex n` such that 
`all f seqt' == True` and `all f' seqf' == True`, where 
`f' x = ~ f x` and ``(seqt', seqf') = splitAt`{i} seq'``.  Use the 
`partition_rearranges` property to `:prove` (or if you lose 
patience, `:check`) that you defined `partition` correctly.

```cryptol
/**
 * "Partition" a sequence `seq` by a filtering predicate `f` such 
 * that the output `seq'` has all the items satisfying `f`, followed 
 * by all items not satisfying `f`
 */
partition: {n, a} (fin n) => (a -> Bit) -> [n]a -> [n]a
partition f seq = undefined
```

```cryptol
/** `partition` (with arguments) is equivalent to `rearrange` */
partition_rearranges: {n} (fin n) => String n -> Bit
partition_rearranges =
    partition isNotPadding === rearrange
      where
        isNotPadding c = c != '-'
```

**EXERCISE**: Is it possible to programmatically determine `i` for an 
arbitrary sequence `seq` and predicate `f`?  Why (not)?  Regardless, 
how might we be able to use `partition` for transposition ciphers?

### Rotate-Partitioning

Another possible approach is to walk through a sequence, rotating the 
remaining subsequence left iff its first element does not satisfy the 
filtering predicate.  Visualizing this for `rearrange` on 
`"HE-LL-O-"`, this approach would proceed as follows:

"HE-LL-O-"
"HELL-O--"
"HELLO---"

To pull this off would require a way to enumerate over subsequences 
split before and after a current index, e.g.

("", "HE-LL-O-")
("H", "E-LL-O-")
("HE", "-LL-O-") -> ("HE", "LL-O--")
...

**EXERCISE**: Is it possible to enumerate over sequence indices and 
pass each index `i` to compute ``splitAt`{i} seq``?  Why (not)?  If 
it is possible, feel free to submit a pull request!  Otherwise, can 
we apply another advanced technique to operate over these 
subsequences?

Unfortunately, there is no mechanism for "type sequence 
comprehensions".  However, we can apply recursion...

```cryptol
/**
 * Recursively shift `-` characters to the end of a string using 
 * sequence rotation
 */
rearrange':
    {n} fin n =>
    String n -> String n
rearrange' w =
    if `n == (0: [width n]) then w
    | (w # ['\0']) @ 0 == '-' then rearrange' (take`{max 1 n - 1} (w <<< 1)) # (take`{min 1 n} ['-'])
    else (take`{min 1 n} [(w # ['\0']) @ 0]) # rearrange' (drop`{min 1 n} w)

/** The iterative and recursive `rearrange(')` functions are equivalent */
rearrange_equiv:
    {n} fin n =>
    String n -> Bit
rearrange_equiv = rearrange`{n} === rearrange'`{n}
```

In addition to being recursive, this approach confusingly adds and 
removes an arbitrary `\0` character and applies tricks with `min` and 
`max` to establish type consistency for an empty sequence.  What kind 
of fool thought this up?  (See [Intro to Type Hackery](labs/Language/IntroTypeHackery.md) 
for a better, more detailed example of such hackery...when this lab 
gets merged into the repo...)

**EXERCISE**: Using `rearrange'` as a blueprint, define a function 
`partition'` that does the same as `partition`, and try to convince 
yourself (via `:prove` and/or `:check` commands using 
`partition'_rearranges`) that your definition of `partition'` is 
correct and equivalent to `partition` for various sequence sizes and 
types.

```cryptol
partition':
    {n, a} fin n =>
    (a -> Bit) -> [n]a -> [n]a
partition' f w = undefined
```

```cryptol
/** `partition'` (with arguments) is equivalent to `rearrange'` */
partition'_rearranges : {n} (fin n) => String n -> Bit
partition'_rearranges =
    partition' isNotPadding === rearrange'
      where
        isNotPadding c = c != '-'
```

## Reduction of Padded Partition Mappings

Phew!  Now that we have defined a `partition` function...

**EXERCISE**: Define a function `unpad` that uses `partition` (or 
`partition'`) and `take` to reduce a `PermutationMapping (n + p)` to 
a possibly smaller `PermutationMapping n` (where `p >= 0`).  Use 
`unpad_unpads` to check your definition of `unpad` is correct for 
various _valid permutation mappings_ of various lengths and paddings.  
(Checking invalid permutation mappings is trivial and inefficient.)  
Can you think of a more efficient way to increase confidence in the 
correctness of this function?

```cryptol
unpad:
    {n, p} (fin n, fin p) =>
    PermutationMapping (n + p) -> PermutationMapping n
unpad pi = undefined
```

```cryptol
unpad_unpads:
    {n, p} (fin n, fin p) =>
    PermutationMapping (n + p) -> Bit
unpad_unpads pi =
    isPermutationMapping pi ==> isPermutationMapping`{n} (unpad pi)
```

# Conclusion

This lab presented abstract definitions for transposition ciphers, 
formalizing definitions for permutations and inverses.  Subsequent 
labs will provide specific examples of transposition ciphers.

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

Up: [Course README](README.md)
Previous: [Common Properties for Ciphers](CommonProperties.md)
Next: [Esrever: A trivial message-reversing transposition "cipher"](Esrever.md)
