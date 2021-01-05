# Introduction

This module asks you to specify the [Route](https://en.wikipedia.org/wiki/Transposition_cipher#Route_cipher)
transposition cipher, in which a message is "routed" around a grid in 
a spiral pattern.  This cipher is very simple on paper, but might be 
more of a challenge to specify in Cryptol.  

## Prerequisites

Before working through this lab, you'll need 
  * Cryptol to be installed and
  * this module to load successfully.

You'll also need experience with
  * loading modules and evaluating functions in the interpreter and
  * the `:prove` and `:sat` commands
  * ...

For a background on Route, we recommend perusing the Wikipedia 
entries on [transposition ciphers](https://en.wikipedia.org/wiki/Transposition_cipher) 
in general and [Route](https://en.wikipedia.org/wiki/Transposition_cipher#Route_cipher) 
in particular.

## Skills You'll Learn

This is an independent exercise.  Who knows what you'll learn?

## Load This Module

This lab is a
[literate](https://en.wikipedia.org/wiki/Literate_programming) 
Cryptol document --- that is, it can be loaded directly into the 
Cryptol interpreter. Load this module from within the Cryptol 
interpreter running in the `cryptol-course` directory with:

```Xcryptol-session
Cryptol> :m labs::Transposition::Route
Loading module Cryptol
Loading module specs::Primitive::Symmetric::Cipher::Block::Cipher
Loading module specs::Primitive::Symmetric::Cipher::Block::DES
Loading module labs::CryptoProofs::CryptoProofs
Loading module labs::Transposition::CommonProperties
Loading module labs::Transposition::Transposition
Loading module labs::Transposition::Route
```

Start by defining the module for this lab:

```cryptol
module labs::Transposition::Route where
```

You should import your common definitions for transposition ciphers:

```cryptol
import labs::Transposition::Transposition
```

# Route

**EXERCISE**: Define `pi` and `pi_test`.  The aforementioned 
Wikipedia article presents an example.  `pi_test` should include it.

# Conclusion

This lab asked you to specify the Route transposition cipher.  Now 
you have at least one independently written spec under your belt.  Go 
forth and do good!

# Solicitation

How was your experience with this lab? Suggestions are welcome in the
form of a ticket on the course GitHub page:
https://github.com/weaversa/cryptol-course/issues

# From here, you can go somewhere!

||||
|-:|:-:|-|
|| [ ^ Transposition Ciphers](./Contents.md) ||
| [< Rail Fence](./RailFence.md) | **Route** ||
