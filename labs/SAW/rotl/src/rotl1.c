#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

uint32_t rotl(uint32_t bits, uint32_t shift) {
  return (bits << shift) | (bits >> (sizeof(bits) * 8 - shift));
}
