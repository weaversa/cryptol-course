# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. Load this module from within the Cryptol interpreter running
in the `cryptol-course` directory with:

```shell
cryptol> :m labs::SimonSpeck::SimonSpeck
```
This lab will introduce the student to the use of Cryptol's 
**Parameterized Modules** through the [Simon](https://en.wikipedia.org/wiki/Simon_(cipher)) and [Speck](https://en.wikipedia.org/wiki/Speck_(cipher)) algorithms.



This lab will take the student through developing wrapping algorithms
described in [NIST Special Publication
800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final)
"Recommendation for Block Cipher Modes of Operation: Methods for Key
Wrapping". We recommend you have this lab and the specification
document open side-by-side.



Since we are creating a new module, the first line needs to be the
module definition:

```
module labs::KeyWrapping::KeyWrapping where
```

# References

