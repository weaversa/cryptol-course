#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

uint32_t RCS(uint32_t bits, uint32_t shift) {
  shift %= 64;
  if(shift == 0) return bits;
  return (bits << shift) | (bits >> (sizeof(bits)*8 - shift));
}

int main() {
  return 0;
}
