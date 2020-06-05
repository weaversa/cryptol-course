# Salsa20 Security Properties

In this lab, we consider additional properties of the [Salsa20]
(Salsa20Spec.md) stream cipher, which has undergone much scrutiny
since being proposed for [eSTREAM, the ECRYPT Stream Cipher Project]
(https://www.ecrypt.eu.org/stream).

```
module labs::Salsa20::Salsa20Props where

import labs::Salsa20::Salsa20Answers
```

## Invertibility

Throughout [the original spec](Salsa20Spec.pdf), various functions are
noted as being "invertible". In the [Salsa20 lab](Salsa20Answers.md),
we proved that the `quarterround` and `littleendian` functions are
invertible. We did this because the specification explicitly mentioned
these properties. Similarly, though not called out in the
specification, other functions defined in the spec are also
invertible.

**EXERCISE**: Below is a rundown of each function defined in the
spec. Your goal is to determine which other functions are invertible
by filling in a roperty for each that attempts to prove invertibility.

```
property rowroundIsInvertibleProp y y' = False
```

```
property columnroundIsInvertibleProp x x' = False
```

This next one (`doubleround`) may take about a minute to prove using
the `z3` solver. It may seem a bit frustrating to have to wait, but
please keep in mind that the solvers are searching over a space of
about `2^^1024` possibilities.

```
property doubleroundIsInvertibleProp xs xs' = False
```

```
property Salsa20CoreIsInvertibleProp x x' = False
```

`Salsa20Expansion` takes two different key sizes and the key is also
viewed as a fixed parameter in terms of invertiblity.

```
property Salsa20ExpansionIsInvertibleProp k n n' = False
```

```
property Salsa20EncryptIsInvertibleProp k v m m' = False
```


## `Salsa20_encrypt`

Finally, we show that `Salsa20_encrypt` is invertible (there exists a
`Salsa20_decrypt` function that inverts `Salsa20_encrypt` for a given
key and nonce) and
[involutive](https://en.wikipedia.org/wiki/Involution_(mathematics)
(applying `Salsa20_encrypt` twice for a given key and nonce yields the
original plaintext).

**EXERCISE**: Define involution.

```
involutive :
    {a}
    () => /* REPLACE WITH YOUR DEFINITION */
    Unary a a ->
    a -> Bit
involutive f =
    FalseUnary  /* REPLACE WITH YOUR DEFINITION */
```

**EXERCISE**: Specify that `Salsa20_encrypt` is involutive:

```
Salsa20_encrypt_involutive :
    {l}    /* REPLACE WITH YOUR DEFINITION */
    () =>  /* REPLACE WITH YOUR DEFINITION */
    /* REPLACE WITH YOUR DEFINITION */
    Bytes l -> Bit
Salsa20_encrypt_involutive m =
    involutive clear m  /* REPLACE WITH YOUR DEFINITION */

/* UNCOMMENT
property Salsa20_encrypt_involutive_1_1 = Salsa20_encrypt_involutive`{1,1}
property Salsa20_encrypt_involutive_1_8 = Salsa20_encrypt_involutive`{1,8}
property Salsa20_encrypt_involutive_1_64 = Salsa20_encrypt_involutive`{1,64}
property Salsa20_encrypt_involutive_1_128 = Salsa20_encrypt_involutive`{1,128}

property Salsa20_encrypt_involutive_2_1 = Salsa20_encrypt_involutive`{2,1}
property Salsa20_encrypt_involutive_2_8 = Salsa20_encrypt_involutive`{2,8}
property Salsa20_encrypt_involutive_2_64 = Salsa20_encrypt_involutive`{2,64}
property Salsa20_encrypt_involutive_2_128 = Salsa20_encrypt_involutive`{2,128}
*/
```

**EXERCISE**: Given that `Salsa20_encrypt` is involutive, it is
trivial to define and specify its inverse:

```
/**
 * inverse of `Salsa20_encrypt`
 */
/* Salsa20_decrypt :
 *     UNCOMMENT AND REPLACE WITH YOUR DEFINITION */
Salsa20_decrypt k v m =
    zero  /* REPLACE WITH YOUR DEFINITION */

/**
 * `Salsa20_decrypt` inverts `Salsa20_encrypt`.
 */
/* Salsa20_encrypt_recovery :
       UNCOMMENT AND REPLACE WITH YOUR DEFINITION */
Salsa20_encrypt_recovery k v m =
    FalseBinary  /* REPLACE WITH YOUR DEFINITION */

/* UNCOMMENT
property Salsa20_encrypt_recovery_1_1 = Salsa20_encrypt_recovery`{1,1}
property Salsa20_encrypt_recovery_1_8 = Salsa20_encrypt_recovery`{1,8}
property Salsa20_encrypt_recovery_1_64 = Salsa20_encrypt_recovery`{1,64}
property Salsa20_encrypt_recovery_1_128 = Salsa20_encrypt_recovery`{1,128}

property Salsa20_encrypt_recovery_2_1 = Salsa20_encrypt_recovery`{2,1}
property Salsa20_encrypt_recovery_2_8 = Salsa20_encrypt_recovery`{2,8}
property Salsa20_encrypt_recovery_2_64 = Salsa20_encrypt_recovery`{2,64}
property Salsa20_encrypt_recovery_2_128 = Salsa20_encrypt_recovery`{2,128}
*/
```


# Collisions

Dr. Bernstein clarifies, "I originally introduced the Salsa20 core as
the "Salsa20 hash function," but this terminology turns out to confuse
people who think that "hash function" means "collision-resistant
compression function."  The Salsa20 core does not compress and is not
collision-resistant." [1]

This was in response [2] to a paper [3] detailing collisions in
Salsa20, which Dr. Bernstein calls "plagiarism of an observation that
was made by Matt Robshaw in June 2005, that was independently posted
to `sci.crypt` by David Wagner in September 2005 [4]..."  High drama!

That Salsa20 (core) does not compress is obvious from its type
signature (`Bytes 64 -> Bytes 64`).  Let's explore its collisions
using Cryptol...


## General Definitions

First, consider what it means for a function of one argument to
*collide*.

**EXERCISE**: Define `Collidable` to specify a constraint over the
unary function argument to determine whether it can collide.

```
/** constraints under which a unary function from `a` to `a'` can collide */
type constraint Collidable a a' =
    (Zero a, Zero a')  /* REPLACE WITH YOUR DEFINITION */
```

**EXERCISE**: Define `collides` to return `True` iff a unary function
`f` returns the same value for arguments `x` and `x'`:

```
/** whether unary function `f` returns the same value for `x` and `x'` */
collides :
    {a, a'}
    Collidable a a' =>
    Unary a a' ->
    a -> a -> Bit
collides f x1 x2 =
    False  /* REPLACE WITH YOUR DEFINITION */
```

Now we can apply these definitions to components of Salsa20...

```
/**
 * whether unary function `f` differs for all different inputs `x1` and `x2`
 */
has_no_collisions :
    {a, a'}
    Collidable a a' =>
    Unary a a' ->
    a -> a -> Bit
has_no_collisions f x1 x2 =
    ~ (collides f x1 x2)

/** `quarterround` has no collisions. */
quarterround_has_no_collisions :
    Words 4 -> Words 4 -> Bit
property quarterround_has_no_collisions =
    has_no_collisions quarterround
```

## `rowround`, `columnround`  

**EXERCISE**: Introduce properties specifying whether `rowround` and
`columnround` are each collision-free:

```
/** `rowround` has no collisions. */
rowround_has_no_collisions :
    Words 4 -> Bit
property rowround_has_no_collisions =
    FalseUnary  /* REPLACE WITH YOUR DEFINITION */

/** `columnround` has no collisions. */
columnround_has_no_collisions :
    Words 4 -> Bit
property columnround_has_no_collisions =
    FalseUnary  /* REPLACE WITH YOUR DEFINITION */
```

## `doubleround`

**EXERCISE**: Is `doubleround` also collision free?  Specify a
property stating whether this is the case, and use Cryptol to prove it
or find a counterexample:

```
/** `doubleround` has no collisions. */
doubleround_has_no_collisions :
    Words 4 -> Bit
property doubleround_has_no_collisions =
    FalseUnary  /* REPLACE WITH YOUR DEFINITION */
```

## `Salsa20`

Depending on the platform, `> :prove doubleround_has_no_collisions`
may have taken a while...  Given the definition of `Salsa20` and the
invertibility and collision properties established to this point, can
you conclude whether or not `Salsa20` is collision-free?  Can Cryptol
just tell us?  Let's find out...

**EXERCISE**: Using `:check` and `:prove`, try to determine whether
`Salsa20` is collision-free.  While you await an answer, consider the
question yourself.  Reading [2]-[4] should pass the time nicely...

```
/** `Salsa20` has no collisions...or does it? */
Salsa20_has_no_collisions :
    Bytes 64 -> Bytes 64 -> Bit
property Salsa20_has_no_collisions =
    has_no_collisions Salsa20Core
```

When you are ready to move on, interrupt Cryptol (`Ctrl-C`) (unless it
has reported unsatisfiability or a counterexample).

Spoiler: `Salsa20` collides.  This is the function that inspired the
aforementioned drama.  Working from [3], Theorem 6 states the
collision property for the `Salsa20` core function:

> Any pair of inputs M and M' (defined below) such that `Z < 2^^31`
> and `Z' = Z + 2^^31`, generate a collision for any number of rounds
> of the `Salsa20` "hash" [core] function, producing `h` (defined
> below) as a common hash value.
> 
> M =  [  Z , -Z ,  Z , -Z
>      , -Z ,  Z , -Z ,  Z
>      ,  Z , -Z ,  Z , -Z
>      , -Z ,  Z , -Z ,  Z  ]
> M' = [  Z', -Z',  Z', -Z'
>      , -Z',  Z', -Z',  Z'
>      ,  Z', -Z',  Z', -Z'
>      , -Z',  Z', -Z',  Z' ]
> h  = 2*M

In this definition, `Z` and `Z'` are `Word`s, but our Cryptol spec
follows the original in defining `Salsa20` to map `Bytes 64 ->
Bytes 64`.  Thus, we will reshape `Salsa20` before proceeding:

**EXERCISE**: Define `Salsa20'` to mimic `Salsa20`, but over
`Words 16` rather than `Bytes 64`: (Hint: This can be defined in
one line with `doubleround`, `iterate`, and basic Cryptol operators.)

```
/** `Salsa20` equivalent over `Words 16` */
Salsa20' :
  Words 16 -> Words 16
Salsa20' x =
  zero  /* REPLACE WITH YOUR DEFINITION */
```

**EXERCISE**: Verify that `Salsa20'` agrees with `Salsa20`:

```
Salsa20'_equiv_Salsa20 :
    Words 16 -> Bit
Salsa20'_equiv_Salsa20 w =
    Salsa20' w == dejigger (Salsa20Core (rejigger w))
      where
        rejigger x = zero  /* REPLACE WITH YOUR DEFINITION */
        dejigger x = zero  /* REPLACE WITH YOUR DEFINITION */
```

**EXERCISE** Formalize Theorem 6 :

```
Salsa20'_collides_Th6 :
    [32] -> Bit
property Salsa20'_collides_Th6 Z =
    collides Salsa20' M M' /\
    h == (zero : Words 16)  /* REPLACE WITH YOUR DEFINITION */
      where
        Z' = Z  /* REPLACE WITH YOUR DEFINITION */
        M  = [  Z , -Z ,  Z , -Z
             , -Z ,  Z , -Z ,  Z
             ,  Z , -Z ,  Z , -Z
             , -Z ,  Z , -Z ,  Z ]
        M' = zero  /* REPLACE WITH YOUR DEFINITION */
        h  = zero  /* REPLACE WITH YOUR DEFINITION */
        h' = h  /* REPLACE WITH YOUR DEFINITION */

```

Theorem 7 [3] states a corresponding [2nd preimage attack]
(https://en.wikipedia.org/wiki/Preimage_attack):

> Any pair of inputs `A, B` with a difference `A - B = A ^ B == D` (defined 
> below) will produce the same output [of `Salsa20`] over any number of rounds.
> 
> A - B = A ^ B = [ 0x80000000, 0x80000000, 0x80000000, 0x80000000
>                 , 0x80000000, 0x80000000, 0x80000000, 0x80000000
>                 , 0x80000000, 0x80000000, 0x80000000, 0x80000000
>                 , 0x80000000, 0x80000000, 0x80000000, 0x80000000 ]

We might ask Cryptol to convince us whether this property holds, but
before proceeding with the general theorem, let's first address the
case of one round:

```
/** Theorem 7 [3] holds for one round. */
Salsa20'_collides_Th7_oneround :
    Words 16 -> Bit
property Salsa20'_collides_Th7_oneround x =
    Salsa20' x == Salsa20' zero  /* REPLACE WITH YOUR DEFINITION */
```

Trying to `:prove` this might have taken a while.  Let's cautiously
move on to the general case.

**EXERCISE** State and try to `:prove` Theorem 7 for any number of
rounds.

```
/** Theorem 7 [3] holds for any number of rounds...well, _almost_ any... */
Salsa20'_collides_Th7 :
    {n}
    (fin n) =>
    [n] -> Words 16 -> Bit  /* REPLACE WITH YOUR DEFINITION */
Salsa20'_collides_Th7 i x =
    True \/                         /* REPLACE WITH YOUR DEFINITION */
    iterate Salsa20' x @ i == zero  /* REPLACE WITH YOUR DEFINITION */

property Salsa20'_collides_Th7_2_rounds =
    Salsa20'_collides_Th7 (2 : [2])

property Salsa20'_collides_Th7_3_rounds =
    Salsa20'_collides_Th7 (3 : [2])
```


# Additional Exercises

The original spec remarks on some other interesting properties:
  * **EXERCISE**: Specify and prove that `littleendian` meets
    Dr. Bernstein's arithmetic specification:
    > If `b = (b0, b1, b2, b3)` then
      `littleendian(b) = b0 + 2^8*b + 2^16*b^2 + 2^24*b^3`
  * **EXERCISE**: Define
    [`transpose`](https://en.wikipedia.org/wiki/Transpose) and verify
    that `columnround M == transpose rowround M` (and vice versa)
  * **EXERCISE**: The original spec mentions parallel visualizations
    of `rowround`, `columnround`, and `doubleround`.  Such an
    interpretation of `rowround` follows.  Specify parallel
    interpretations for `columnround` and `doubleround`, and verify
    that these are correct.
  * **EXERCISE** Alternative Salsa20-based stream ciphers (Salsa20/8 and
    Salsa20/12) have been specified to improve performance. Specify these and
    verify basic cryptographic properties such as plaintext recovery.


```
/** parallel "visualization" of `rowround` */
rowround_parallel :
    Words 16 -> Words 16
rowround_parallel ys =
    join [ (quarterround (yi<<<i))>>>i | yi <- split ys | i <- [0 .. 3] : [4][2] ]

/**
 * The parallel "visualization" of `rowround` is equivalent to its explicit 
 * specification.
 */
rowround_parallel_is_rowround :
    Words 16 -> Bit
property rowround_parallel_is_rowround =
    rowround === rowround_parallel
```

# References

[1] [The Salsa20 core]
    (http://cr.yp.to/salsa20.html)
    D. Bernstein

[2] [Response to "On the Salsa20 Core Function"]
    (https://cr.yp.to/snuffle/reoncore-20080224.pdf)
    D. Bernstein

[3] [On the Salsa20 Core Function]
    (https://www.iacr.org/archive/fse2008/50860470/50860470.pdf)
    J.C. Hernandez-Castro, et. al.

[4] [Re-rolled Salsa20 function]
    (https://groups.google.com/d/msg/sci.crypt/AkQnSoO40BA/o4eG96rjkgYJ)
    P. Rubin, D. Wagner, et. al.

[5] [Salsa20 specification]
    (https://cr.yp.to/snuffle/spec.pdf)
    D. Bernstein

```
private

    /** property that is always `False` for one argument */
    FalseUnary x = False
    
    /** property that is always `False` for two arguments */
    FalseBinary x y = False
    
    /** clearing function */
    clear : {a} (Zero a) => Unary a a
    clear x = zero

```
