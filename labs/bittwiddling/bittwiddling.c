#include<stdint.h>
#include<stdbool.h>

/* Here are a set of functions for you to verify.
   They are optimized code for doing relatively simple
   bit manipulations.  There were taken from the
   "Bit Twiddling Hacks" website created by Sean Eron Anderson.
   To compile this program, run:
   clang -c -emit-llvm bittwiddling.c -o bittwiddling.bc
   This code should be loaded into SAW and compared against 
   the specifications you wrote in a Cryptol program.
*/

/* Determine the parity of a byte.
   Returns true if it has odd parity
   Returns false if it has even parity */
bool parity8bit(unsigned char b){
  return (((b * 0x0101010101010101ULL) & 0x8040201008040201ULL) % 0x1FF) & 1;
}

/* Determine the parity of a 32 bit word.
   Returns 1 if it has odd parity
   Returns 0 if it has even parity */
uint32_t parity32bit(uint32_t v){
    v ^= v >> 1;
    v ^= v >> 2;
    v = (v & 0x11111111U) * 0x11111111U;
    return (v >> 28) & 1;
}

/* Determine the parity of a 64 bit word.
   Returns 1 if it has odd parity
   Returns 0 if it has even parity */
uint64_t parity64bit(uint64_t v){
    v ^= v >> 1;
    v ^= v >> 2;
    v = (v & 0x1111111111111111UL) * 0x1111111111111111UL;
    return (v >> 60) & 1;
}

/* Reverses the bit order of a byte */
unsigned char reversebyte(unsigned char b){
  return (b * 0x0202020202ULL & 0x010884422010ULL) % 1023;
}

/*Checks if any of the 4 bytes of
  a 32 bit word is zero.
  Returns true if any byte is zero,
  returns false otherwise. */
bool anyZeroByte(uint32_t v){
  return ~((((v & 0x7F7F7F7F) + 0x7F7F7F7F) | v) | 0x7F7F7F7F);
}
