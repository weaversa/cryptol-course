# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. 

This lab will work through developing the key wrapping algorithms
described in [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) "Recommendation for Block Cipher 
Modes of Operation: Methods for Key Wrapping". We recommend you
have this lab and the specification document open side-by-side.

Here is the abstract from this document:

> This publication describes cryptographic methods that are approved for “key 
> wrapping,” i.e., the protection of the confidentiality and integrity of 
> cryptographic keys. In addition to describing existing methods, this 
> publication specifies two new, deterministic authenticated-encryption modes of
> operation of the Advanced Encryption Standard (AES) algorithm: the AES Key
> Wrap (KW) mode and the AES Key Wrap With Padding (KWP) mode. An analogous mode
> with the Triple Data Encryption Algorithm (TDEA) as the underlying block 
> cipher, called TKW, is also specified, to support legacy applications.

We will develop the three main algorithms -- `KW`, `KWP`, and `TKW` -- by 
building up the necessary subcomponents.

First, since we are creating a new module, the first line needs to be the
module definition:

```
module labs::KeyWrapping::KeyWrapping where
```

# Getting Started

The [NIST Special Publications 800 Series](https://www.nist.gov/itl/publications-0/nist-special-publication-800-series-general-information) provide information of interest to
the computer security community. The series comprises guidelines, 
recommendations, technical specifications, and annual reports of NIST’s 
cybersecurity activities.

Reading through and implementing a formal specification in Cryptol for one of 
the cryptography standards in this series can be a challenge. The standards are 
written by a variety of authors and the algorithms are often described in 
language-agnostic pseudo code which we have to parse. This translation process
can lead to subtle implementation errors which can be hard to spot.

Our job is to extract the relevant details from the specifictation and build
Cryptol specifications for the three main algorithms listed above.


# Preliminaries

The Key Wrapping algorithms described in this document are a form of [Block Cipher Mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation) for an existing
block cipher. Section 5.1 (p. 8) of the standard states:

> For KW and KWP, the underlying block cipher shall be approved, and the block 
> size shall be 128 bits. Currently, the AES block cipher, with key lengths of 
> 128, 192, or 256 bits, is the only block cipher that fits this profile. For 
> TKW, the underlying block cipher is specified to be TDEA, and the block size 
> is therefore 64 bits; the KEK for TKW may have any length for which TDEA is
> approved; see [8].

The algorithms `KW` and `KWP` are defined to operate with [`AES`](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) for the three key sizes: 128, 192, and 256 bits. 

The standard does not indicate a specific key size for [`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), so we will use the **CHECK** standard ??? bit key size.

We will not be developing `AES` or `TDEA` in this lab. Instead we will use
pre-written modules which provide these block ciphers. The algorithms are found
under the `specs/` directory in `specs/Primitive/Symmetric/Cipher/Block`
and we load them into our module with the following:

```
import specs::Primitive::Symmetric::Cipher::Block::AES128 as AES128
import specs::Primitive::Symmetric::Cipher::Block::AES192 as AES192
import specs::Primitive::Symmetric::Cipher::Block::AES256 as AES256
```

**TODO** Discuss how to call the encrypt/decrypt functions for these...

Cryptol modules must match the directory structure they reside in. Using
descriptive names as we do here is a good way to organize related algorithms
by type, function, or whatever works for your system.

Now is a good time to scan through the document and get a sense of the overall organization

 * **Sections 1 - 3, Purpose, Authority, and Introduction** -- These sections provide background, usage, and cryptographic function of the algorithms described in this document. This information is good background if we were trying to decide *how* to use these algorithms; however we will not need to reference this information to build our specifications.
 
 Feel free to skim through this material or skip for now.

 * **Section 4, Definitions and Notation** -- This section contains important definitions, acronyms, variables, and operations used in this standard. Let's skim through this to see if we find anything useful...
 
Section `4.3` looks like it has some constants `ICV1`, `ICV2`, and `ICV3` which are defined to have special values. Since we are working inside of our own module we can define these variables without fear of polluting another namespace:

```
ICV1 = 0xA6A6A6A6A6A6A6A6
ICV2 = 0xA65959A6
ICV3 = 0xA6A6A6A6
```

Section `4.4` introduces operators and notation for cryptographic functions and their building blocks. We have imported the required block ciphers, we will be building some of these for ourselves, and the remainder Cryptol provides for us in some fashion or another.

 * **Section 5, Preliminaries** -- **TODO** This is a bit more complicated section about data restrictions.
 
 * **Section 6, Specifications of KW and KWP** -- This section specifies the two primary algorithms `KW` and `KWP`. This section is the reference we will use for the bulk of this lesson.
 
 * **Section 7, Specification of TKW** -- This section specifies the final major algorithm `TKW`. It is structurally very similar to `KW`, but there are some differences that warrant its own section.
 
 * **Section 8, Conformance** -- This section has information about how implementations may claim conformance to the algorithms described in this standard.
 
 
# Writing `KW`

Let's 
 
# The end

How was your experience with this lab? Suggestions are welcome in the form of a 
ticket on the course Github page: [https://github.com/weaversa/cryptol-course/issues](https://github.com/weaversa/cryptol-course/issues)

# References

* [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) -- "Recommendation for Block Cipher Modes of Operation: Methods for Key Wrapping". This document is the primary source material for this lab. It contains the original specifications from NIST that we develop in this lesson.

* [RFC 3394](https://tools.ietf.org/html/rfc3394) -- "Advanced Encryption Standard (AES) Key Wrap Algorithm". This RFC contains information on the same set of algorithms and includes test vectors which we use in this lab to verify that our algorithms were implemented correctly.
