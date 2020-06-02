# Bit Twiddling Exercises.

Sean Eron Anderson, a former graduate student at Stanford, collected a large set of [Bit Twiddling Hacks]{https://graphics.stanford.edu/~seander/bithacks.html}.  This collection has a large number of algorithms which perform relatively simple calculations or test properties (e.g. determine the absolute value) in surprising ways.  The code is often opaque; that is, it's not easy to see why (or even if) they actually work.  This is a good example of how we can use Cryptol and SAW!  With Cryptol, we can write code that is easy to read, and as such we have high confidence it is correct.  With SAW, we can compare the Cryptol code against the inscrutable bit twiddling code.

This file will contain your code (written in Cryptol, naturally) that you use to validate the correctness of the code written in the corresponding C file (bittwiddling.c).  You should look at the C code to help you figure out the type signature of your Cryptol functions.  For example, does the C function return a Boolean value (True or False) or a number?  If a number, how many bits is it?  Your Cryptol code shouldn't match the C code, but instead should be relatively simple (some are as simple as calling a function that already exists in Cryptol!), and they should all be relatively easy to read.  We also included some properties to help test your code.  After you have completed this code, you should write (from scratch) a SAWscript to compare the two.

## Parity Exercises

In mathematics, the [parity]{https://en.wikipedia.org/wiki/Parity_(mathematics)} of an integer simply says whether it is even or odd.  For a set of bits, we extend this definition to finding the parity of the sum of the bits.  For example, the parity 0b0011 is even (the sum of the bits is 2) while 0b1110 is odd (the sum of the bits is 3).  Rather than returning "even" or "odd", we can return "0" if it's even or "1" if it's odd; since "0" often represents "false" on a computer and "1" represents "true", the parity function may return one of the Boolean values.

A straightforward way to calculate the parity of a set of bytes is to XOR all the bits together: if there's an even number of 1s, the result is 0 (or false), and if there's an odd number of 1s, the result is 1 (or true).  We will compare three different functions: one for a byte (8 bits), one for a word (32 bits), and one for a double word (64 bits).

### Byte parity

Start with a function that finds the parity of a byte.

```
parity8bit : [8] -> Bit
parity8bit b = bs ! 0
  where
   bs = [ False ] # [ bit1 ^ bit2 | bit1 <- b | bit2 <- bs ]

testparity8bit : Bit
property testparity8bit =
    parity8bit 0x01 == True  /\
    parity8bit 0x00 == False /\
    parity8bit 0xff == False /\
    parity8bit 0xef == True
```

### Word parity

Now write a function that finds the parity of a word (the function could look exactly the same as the byte function).

```
parity32bit : [32] -> [32]
parity32bit b = zero # [bs ! 0]
  where
   bs = [ False ] # [ bit1 ^ bit2 | bit1 <- b | bit2 <- bs ]

testparity32bit : Bit
property testparity32bit =
    parity32bit 0x00000001 == 1 /\
    parity32bit 0x00000000 == 0 /\
    parity32bit 0xffffffff == 0 /\
    parity32bit 0x7fffffff == 1
```

### Double word parity

Finally, write a function that finds the parity of a double word (again, the code could be the same as the previous two exercises).

```
parity64bit : [64] -> [64]
parity64bit b = zero # [bs ! 0]
  where
   bs = [ False ] # [ bit1 ^ bit2 | bit1 <- b | bit2 <- bs ]

testparity64bit : Bit
property testparity64bit =
    parity64bit 0x0000000000000001 == 1 /\
    parity64bit 0x0000000000000000 == 0 /\
    parity64bit 0xffffffffffffffff == 0 /\
    parity64bit 0xffffffff7fffffff == 1
```

## Reverse a byte

Write a function which takes a byte and reverses it.  For example, 0b10101010 should map to 0b01010101.

```
reverseByte : [8] -> [8]
reverseByte b = reverse b

testreverseByte : Bit
property testreverseByte =
    reverseByte 0x01 == 0x80 /\
    reverseByte 0xaa == 0x55
```

## Check if a 32 word has a zero byte

Write a function which takes a word (32 bits, i.e. four bytes) and checks if any of the bytes is all zeroes.  Note that it has to be on the byte boundaries and not just anywhere.  For example, 0x11001111 has a zero byte (the second byte) but 0x10011111 does not (the first byte is 0x10 while the second byte is 0x01).

```
anyZeroByte : [32] -> Bit
anyZeroByte v = bytecheck != 0
  where
   bytes = split`{4} v
   bytecheck = [ b == 0 | b <- bytes ]

testanyZeroByte : Bit
property testanyZeroByte =
    anyZeroByte 0x10011001 == False /\
    anyZeroByte 0x00112233 == True
```