# Introduction

This lab is a [literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the Cryptol
interpreter. 

This lab will work through developing the key wrapping algorithms
described in [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) "Recommendation for Block Cipher 
Modes of Operation: Methods for Key Wrapping". We recommend you
have this lab and the specification document open side-by-side.

Here is the abstract from this document

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

First, since we are creating a module, the first line needs to be the
module definition.

```
module labs::KeyWrapping::KeyWrapping where

import specs::Primitive::Symmetric::Cipher::Block::AES::TripleDES
```

# Getting Started

[NIST Special Publications](https://www.nist.gov/itl/publications-0/nist-special-publication-800-series-general-information) in the 800 Series provide 
information of interest to the computer security community. The series 
comprises guidelines, recommendations, technical specifications, and annual
reports of NIST’s cybersecurity activities.

Reading through and implementing a formal specification in Cryptol for one of 
these cryptography standards can be a challeng. The standards are written by
a variety of authors with the algorithms often described in an implementation language agnostic pseudo-code.

Our job is to extract the relevant details 




# Preliminaries

The Key Wrapping algorithms described in this document are a form of [Block Cipher Mode](https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation) for an existing
block cipher. Section 5.1 (p. 8) of the standard states:

> For KW and KWP, the underlying block cipher shall be approved, and the block 
> size shall be 128 bits. Currently, the AES block cipher, with key lengths of 
> 128, 192, or 256 bits, is the only block cipher that fits this profile. For 
> TKW, the underlying block cipher is specified to be TDEA, and the block size 
> is therefore 64 bits; the KEK for TKW may have any length for which TDEA is
> approved; see [8].

The necessary block ciphers are to be [`AES`](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) -- with key sizes 128, 192, and 256 bits. The 
standard does not indicate a specific key size for [`TDEA`](https://en.wikipedia.org/wiki/Triple_DES), so we will use the **TODO** standard 168 bit key size.

Will will not be developing these algorithms in this lab. Instead we will use
pre-written modules which provide these block ciphers. The algorithms are found
in **TODO** and we load them into our module with the following:

**TODO**
```

```

# The end

How was your experience with this lab? Suggestions are welcome in the form of a 
ticket on the course Github page: [https://github.com/weaversa/cryptol-course/issues](https://github.com/weaversa/cryptol-course/issues)

# References

* [NIST Special Publication 800-38F](https://csrc.nist.gov/publications/detail/sp/800-38f/final) -- "Recommendation for Block Cipher Modes of Operation: Methods for Key Wrapping"

* [RFC 3394](https://tools.ietf.org/html/rfc3394) -- Advanced Encryption Standard (AES) Key Wrap Algorithm. This RFC contains information on the same set of algorithms and includes test vectors
