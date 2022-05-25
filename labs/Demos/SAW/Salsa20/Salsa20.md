# Background

Before starting this module, we recommend perusing Galois's excellent
tutorial on
[_Program Verification with SAW_](https://saw.galois.com/intro/index.html).
That tutorial
  * introduces Cryptol and SAW
    (you can skip its setup instructions as their installation test
	should work in whichever environment you set up for this course)
  * specifies and verifies a simple bitcount implementation
  * introduces verification of pointer-based implementations
  * describes and motivates a _compositional_ verification of Salsa20
  * offers an extended exercise on proof maintenance for HMAC

# Uninterpreted Functions

_Program Verification with SAW_ demonstrates compositional verification
using _overrides_ that reflect an implementation's _callgraph_ to ease
the burden of proof for SAW:

> The third argument to `crucible_llvm_verify` is a list of
> `CrucibleMethodSpec` objects. While performing verification, the work
> that was done to construct a `CrucibleMethodSpec` is re-used.
> Specifically, instead of recursively symbolically executing a
> verified function, the prior specification is used as an
> axiomatization of its behavior.

The term _override_ comes from SAW's docstring for `llvm_verify`:

```SAW
sawscript> :h llvm_verify
Description
-----------

    llvm_verify : LLVMModule -> String -> [LLVMSpec] -> Bool -> LLVMSetup () -> ProofScript () -> TopLevel LLVMSpec

Verify the LLVM function named by the second parameter in the module
specified by the first. The third parameter lists the LLVMSpec
values returned by previous calls to use as overrides. The fourth (Bool)
parameter enables or disables path satisfiability checking. The fifth
describes how to set up the symbolic execution engine before verification.
And the last gives the script to use to prove the validity of the resulting
verification conditions.
```

One might wonder whether similar hints can ease the burden of proof by
axiomatizing a similarly complex Cryptol specification. Such hints are
called _uninterpreted functions_, which instruct an interface (
[SBV](http://leventerkok.github.io/sbv/) or
[What4](https://github.com/GaloisInc/what4)) to an underlying SMT
solver to axiomatize a Cryptol symbol definition rather than
recursively expanding its underlying definitions. This is especially
useful for implementations that are derived from or closely match a
Cryptol specification, as is the case for Galois's Salsa20 example.

For instance, the following verification of `s20_doubleround` uses both
overrides and uninterpreted functions:

```SAW
dr <- llvm_verify m "s20_doubleround" [cr,rr] false doubleround_setup w4_unint_z3(['columnround', 'rowround']);
```

`cr` and `rr` refer to previous SAW verification results, whereas
`columnround` and `rowround` refer to definitions imported from
`Salsa20.cry`. Though this just shaves a few more tenths of a second
off verification of Salsa20 (and at worst *increases* proof times),
benefits become obvious for more complex verifications such as that of
AES and others in
[LibCrypto](https://github.com/awslabs/aws-lc-verification/blob/master/SAW/proof/AES/AES.saw),
a formally verified cryptographic library for Amazon Web
Services. (This script provides other advanced instructions to aid
SMT solvers that are beyond the scope of this course.)


## Practice with Uninterpreted Functions and Overrides

Now that we have added uninterpreted functions to our repertoire, let's
apply them to Salsa20...but first, let's more closely examine our
target and the Galois tutorial's progress toward verifying it...

Clang/LLVM includes a command line tool called `opt`. `opt` is
primarily an _optimizer_ (hence its name), but can also be used for
analysis. For now we are interested in its feature to generate a
_callgraph_:

```
> cd labs/Demos/Salsa20
> mkdir build
> clang -c -emit-llvm -Iinclude -o build/salsa20.bc src/salsa20.c
> opt -dot-callgraph -o dev/null build/salsa20.bc
cryptol-course/labs/Demos/SAW/Salsa20$ opt-12 -dot-callgraph -o /dev/null build/salsa20.bc
Writing 'build/salsa20.bc.callgraph.dot'...
```

This produces a [dot](https://graphviz.org/doc/info/lang.html) graph,
which can be converted to an image viewable in a web browser:

```
> dot -Tpng -o build/salsa20.png build/salsa20.bc.callgraph.dot
```

<a href="./salsa20.bc.png">
    <img class="center" src="./misc/salsa20.bc.png" alt="salsa20.bc call graph">
</a>

(`llvm.*` are LLVM "intrinsics" that we can ignore for now.)

We are not aware of any similar tool to graphically depict Cryptol
modules, but here is a manually generated graph for `Salsa20.cry`:

<a href="./Salsa20.cry.png">
    <img class="center" src="./misc/Salsa20.cry.png" alt="Salsa20.cry partial dependency graph">
</a>

(These also call numerous functions from the Cryptol prelude, but we
can ignore most these for now. `rotl` implements `(<<<)`, so we include
it in the graph.)

Finally, here is another manually generated graph that shows which SAW
method specifications are verified against which implementation
functions, using which Cryptol definitions:

<a href="./salsa20.saw.png">
    <img class="center" src="./misc/salsa20.saw.png" alt="salsa20.saw verification/override graph">
</a>

Support for
[visualizing a SAW Remote API for Python script](https://github.com/GaloisInc/saw-script/issues/1664)
is in early development.

The edge labels for `s20_encrypt32` represent that it was verified for
different buffer lengths, whereas no other method specifications
require an argument. In general, one could visualize this as a given
edge with source and destination labels representing arguemnts to
respective SAW method specifications. This assumes that each function
in the implementation is specified by a SAW method specification, which
in turn refers to one Cryptol specification that closely reflects the
function being verified. This is often not the case, but if so, shows a
close correspondence between specification and implementation,
simplifying the assurance case.

Informed by these visualizations, perhaps we can fill in some gaps...

**EXERCISE**: Add uninterpreted functions to `llvm_verify` instructions
(replace `abc` with `w4_unint_yices([<uninterpreted functions>])`) in
`salsa20.saw` that are straightforwardly implemented by functions in
`salsa20.bc`. (`s20_quarterround` implements `quarterround`, `s20_hash`
implements `Salsa20`, etc.) How does this affect proof times?

**EXERCISE**: Add a method specification and override to verify that
`rotl` in `salsa20.bc` implements `(<<<)`. Use this override to verify
the function that directly calls `rotl`. How does this affect proof
times? Can you specify `(<<<)` as an uninterpreted function?

**EXERCISE**: Are any potential overrides missing from other
`llvm_verify` instructions in `salsa20.saw`? How does adding these
affect proof times?
