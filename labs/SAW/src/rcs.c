#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

uint32_t RCS(uint32_t bits, uint32_t shift) {
  shift %= sizeof(bits) * 8;
  if(shift == 0) return bits;
  return (bits << (sizeof(bits) * 8 - shift)) | (bits >> shift);
}
