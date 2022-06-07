#include <stdint.h>

uint64_t ceilLog2(uint64_t i) {
  return (i <= 1 ? 0 : 64 - __builtin_clzll(i - 1));
}