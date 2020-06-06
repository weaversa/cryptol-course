# Salsa20 Security Properties

In this lab, we consider additional properties of the [Salsa20 stream
cipher](Salsa20Spec.md) [5], which has undergone much scrutiny since
being proposed for [eSTREAM, the ECRYPT Stream Cipher Project]
(https://www.ecrypt.eu.org/stream).

```
module labs::Salsa20::Salsa20PropsAnswers where

import labs::Salsa20::Salsa20Answers
```

## Invertibility

Throughout [the original spec](Salsa20Spec.pdf) [5], various functions
are noted as being "invertible". In the [Salsa20
lab](Salsa20Answers.md), we proved that the `quarterround` and
`littleendian` functions are invertible. We did this because the
specification explicitly mentioned these properties. Similarly, though
not called out in the specification, other functions defined in the
spec are also invertible.

**EXERCISE**: Below is a rundown of each function defined in the
spec. Your goal is to prove that each are invertible by filling in a
property for each that attempts to prove invertibility.


### rowround

```
property rowroundIsInvertibleProp y y' =
    y != y' ==> rowround y != rowround y'
```


### columnround

```
property columnroundIsInvertibleProp x x' =
    x != x' ==> columnround x != columnround x'
```


### doubleround

This next one (`doubleround`) may take about a minute to prove using
the `z3` solver. It may also seem a bit frustrating to have to wait,
but please keep in mind that the solver is searching over a space of
about `2^^1024` possibilities.

```
property doubleroundIsInvertibleProp xs xs' =
    xs != xs' ==> doubleround xs != doubleround xs'
```


### Salsa20Core

So far we've been proving that functions are invertible by showing
that no two different inputs cause a function's outputs to
_collide_. Though, for `Salsa20Core` we'll actually be trying to find
such collisions.

[Dr. Bernstein states](http://cr.yp.to/salsa20.html) [1],

> I originally introduced the Salsa20 core as the "Salsa20 hash
function," but this terminology turns out to confuse people who think
that "hash function" means "collision-resistant compression function."
The Salsa20 core does not compress and is not collision-resistant.

This was [in response](https://cr.yp.to/snuffle/reoncore-20080224.pdf)
[2] to a [paper detailing collisions in
Salsa20](https://www.iacr.org/archive/fse2008/50860470/50860470.pdf)
[3], which Dr. Bernstein calls

> plagiarism of an observation that was made by Matt Robshaw in June
2005, that was [independently posted to
`sci.crypt`](https://groups.google.com/d/msg/sci.crypt/AkQnSoO40BA/o4eG96rjkgYJ)
[4] by David Wagner in September 2005...

High drama!

The notion that `Salsa20Core` does not compress is obvious from its
type signature (`Bytes 64 -> Bytes 64`). So, we'll work now to show
that `Salsa20Core` collides.

The security of many cryptographic algorithms relies on [collision
resistance](https://en.wikipedia.org/wiki/Collision_resistance). 
Collisions are actually OK in many types of cryptography (think hash 
functions), but collisions should be _astronomically_ hard to find. 
Here, Dr. Bernstein says that collisions exist in `Salsa20Core` and 
are easy to find, though not so easy that `z3` just works out of the 
box.

We know from the section above that `doubleround` is collision free. 
Collision free functions remain so even when iterated (`Salsa20Core` 
iterates `doubleround` ten times). The proof of this last statement 
is left to the reader -- we suggest the [100 prisoners
problem](https://en.wikipedia.org/wiki/100_prisoners_problem) as a
good starting place. Then, where do the collisions in `Salsa20Core`
come from? Looking over the details of `Salsa20Core`, we see
operations that reshape bytes to words and back, but reshaping also
doesn't cause collisions. All that's left is `+`. The original
specification states that

> Salsa20Core x = x + doubleround10 x

To demonstrate the potential for a collision here, consider what 
would happen if `x` was a single bit and `doubleround10 0 == 1` and
`doubleround10 1 == 0`. In this case, `doubleround10` is collision
free, but `Salsa20Core` would return `1` when given `x = 0` and `x =
1` -- a collision. Next we move on to finding such collisions using
the fully defined function.

Working from
[[3]](https://www.iacr.org/archive/fse2008/50860470/50860470.pdf),
Theorem 6 states the collision property for `Salsa20Core`:

> Any pair of inputs M and M' (defined below) such that `Z < 2^^31`
> and `Z' = Z + 2^^31`, generate a collision for any number of rounds
> of the `Salsa20` "hash" [core] function, producing `h` (defined
> below) as a common hash value.
> 
> ```
> M =  [  Z , -Z ,  Z , -Z
>      , -Z ,  Z , -Z ,  Z
>      ,  Z , -Z ,  Z , -Z
>      , -Z ,  Z , -Z ,  Z  ]
> M' = [  Z', -Z',  Z', -Z'
>      , -Z',  Z', -Z',  Z'
>      ,  Z', -Z',  Z', -Z'
>      , -Z',  Z', -Z',  Z' ]
> h  = 2*M
> ```

In this definition, `Z` and `Z'` are `Word`s, but our Cryptol spec
follows the original in defining `Salsa20Core` to map `Bytes 64 ->
Bytes 64`.  Thus, we will reshape `Salsa20Core` before proceeding:

**EXERCISE**: Define `Salsa20Core'` to mimic `Salsa20Core`, but over
`Words 16` rather than `Bytes 64`: (Hint: This can be defined in one
line with `doubleround`, `iterate`, and basic Cryptol operators.)

```
/** `Salsa20Core` equivalent over `Words 16` */
Salsa20Core' : Words 16 -> Words 16
Salsa20Core' x =
    x + (iterate doubleround x @ 10)
```

**EXERCISE**: Verify that `Salsa20Core'` agrees with `Salsa20Core`:

```
property Salsa20CoreEquivProp w =
    Salsa20Core' (rejigger w) == rejigger (Salsa20Core w)
  where
    rejigger x = map littleendian (split x)
```

Theorem 6 states that collisions should happen for any number of
iterations of `Salsa20Core`. However, we're just going to ask that you
prove it for one iteration. If you are highly motivated, feel free to
attempt the more general proof (though don't expect any extra credit).

**EXERCISE** Formalize Theorem 6:

```
Salsa20CoreCollidesProp : [32] -> Bit
property Salsa20CoreCollidesProp Z =
    h == h'
  where
    Z' = Z + (2^^31)
    M  = [  Z , -Z ,  Z , -Z
         , -Z ,  Z , -Z ,  Z
         ,  Z , -Z ,  Z , -Z
         , -Z ,  Z , -Z ,  Z ]
    M' = [  Z', -Z',  Z', -Z'
         , -Z',  Z', -Z',  Z'
         ,  Z', -Z',  Z', -Z'
         , -Z',  Z', -Z',  Z']
    h  = Salsa20Core' M
    h' = Salsa20Core' M'
```


### `Salsa20Encrypt`

Finally, we want to show that `Salsa20Encrypt` is invertible
(i.e. there exists a `Salsa20Decrypt` function that inverts
`Salsa20Encrypt` for a given key and nonce). It turns out that
`Salsa20Encrypt` is it's own inverse, a so-called
[involution](https://en.wikipedia.org/wiki/Involution_(mathematics). That
is, composing `Salsa20Encrypt` twice on a given key and nonce yields
the original plaintext.


**EXERCISE**: Specify that `Salsa20_encrypt` is an involution and use
the myriad of properties below to help verify your work.

```
Salsa20EncryptInvolutionProp :
    {a, l}
    (a >= 1, 2 >= a, l <= 2^^70) =>
    Bytes (16*a) -> Bytes 8 -> Bytes l -> Bit
Salsa20EncryptInvolutionProp k v m =
    Salsa20Encrypt k v (Salsa20Encrypt k v m) == m
```


```
property Salsa20EncryptInvolutionProp_1_1 = Salsa20EncryptInvolutionProp`{1,1}
property Salsa20EncryptInvolutionProp_1_8 = Salsa20EncryptInvolutionProp`{1,8}
property Salsa20EncryptInvolutionProp_1_64 = Salsa20EncryptInvolutionProp`{1,64}
property Salsa20EncryptInvolutionProp_1_128 = Salsa20EncryptInvolutionProp`{1,128}

property Salsa20EncryptInvolutionProp_2_1 = Salsa20EncryptInvolutionProp`{2,1}
property Salsa20EncryptInvolutionProp_2_8 = Salsa20EncryptInvolutionProp`{2,8}
property Salsa20EncryptInvolutionProp_2_64 = Salsa20EncryptInvolutionProp`{2,64}
property Salsa20EncryptInvolutionProp_2_128 = Salsa20EncryptInvolutionProp`{2,128}
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
