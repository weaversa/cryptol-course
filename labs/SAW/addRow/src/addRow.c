#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

void addRow5Mutate(uint32_t a[5], uint32_t b[5]) {
  for(int i = 0; i < 5; i++) {
    a[i] += b[i];
  }
  return;
}

uint32_t* addRow5NewVar(uint32_t a[5], uint32_t b[5]) {
  uint32_t* c = (uint32_t*) malloc(5*sizeof(uint32_t));
  for(int i = 0; i < 5; i++) {
    c[i] = a[i] + b[i];
  }
  return c;
}

uint32_t* addRowAlias(uint32_t* a, uint32_t* b, uint8_t length) {
  for(int i = 0; i < length; i++) {
    a[i] += b[i];
  }
  return a;
}
