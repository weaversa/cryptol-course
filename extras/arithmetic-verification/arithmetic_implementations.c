#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// addition_standard
//
// Add two 16-bit integers in a standard way, cast each to a local 32 bit
// number and then add and return the result.
uint32_t add_standard( uint16_t a, uint16_t b) {
  uint32_t local_a = (uint32_t) a;
  uint32_t local_b = (uint32_t) b;
  uint32_t result;
  
  result = local_a + local_b;
  
  return result;
}

// addition_textbook
//
// Add two 16-bit numbers by treating each as two 8-bit parts and using
// the "textbook" addition algorithm.
//
// Suppose we break up 16-bit integers a and be as follows: 
//     a = [ a_1 ] [ a_0 ] 
//     b = [ b_1 ] [ b_0 ]
//
// Then we compute a + b as:
//                                         [ C_1 ]
//                                         [ a_1 ] [ a_0 ]
//                                       + [ b_1 ] [ b_0 ]
//                                   =====================
//                                 [ O_2 ] [ O_1 ] [ O_0 ]
uint32_t add_textbook( uint16_t a, uint16_t b) {
  uint8_t a_1 = (0xff00 & a) >> 8;
  uint8_t a_0 = (0x00ff & a) >> 0;
  uint8_t b_1 = (0xff00 & b) >> 8;
  uint8_t b_0 = (0x00ff & b) >> 0;

  uint16_t P_0 = a_0 + b_0;
  uint16_t O_0 = P_0 & 0x00ff;
  uint16_t C_1 = (P_0 & 0xff00) >> 8;
  
  uint16_t P_1 = a_1 + b_1 + C_1;
  uint16_t O_1 = P_1 & 0x00ff;
  uint16_t C_2 = (P_1 & 0xff00) >> 8;
  
  uint16_t O_2 = C_2;

  uint32_t result = (O_2 << 16) | (O_1 << 8) | (O_0 << 0);
  
  return result;
}

// multiply_standard
//
// Multiply two 32-bit integers in a standard way, cast each to a local 32 bit
// number and then multiply and return the result.
uint32_t multiply_standard( uint16_t a, uint16_t b) {
  uint32_t local_a = (uint32_t) a;
  uint32_t local_b = (uint32_t) b;
  uint32_t result;
  
  result = local_a * local_b;
  
  return result;
}

// multiply_textbook
//
// Multiply two 16-bit numbers by treating each as two 8-bit parts and using
// the "textbook" multiplication algorithm.
//
// Suppose we break up 32-bit integers a and be as follows: 
//     a = [ a_1 ] [ a_0 ] 
//     b = [ b_1 ] [ b_0 ]
//
// Then we compute a * b as:
//                                         [ a_1 ] [ a_0 ]
//                                       x [ b_1 ] [ b_0 ]
//                                   =====================
//                             [ a_1 * b_0 ] [ a_0 * b_0 ]
//               [ a_1 * b_1 ] [ a_0 * b_1 ]       0
//               =========================================
// [    O_3    ] [    O_2    ] [    O_1    ] [    O_0    ]
//
// Where the outputs are the sums ofhe columns
uint32_t multiply_textbook( uint16_t a, uint16_t b) {
  uint16_t a_1 = (0xff00 & a) >> 8;
  uint16_t a_0 = (0x00ff & a) >> 0;
  uint16_t b_1 = (0xff00 & b) >> 8;
  uint16_t b_0 = (0x00ff & b) >> 0;
  
  uint32_t z0 = a_0 * b_0;
  uint32_t z1 = a_1 * b_0;
  uint32_t z2 = a_0 * b_1;
  uint32_t z3 = a_1 * b_1;
  
  uint32_t result = 0;
  result += z0;
  result += z1 << 8;
  result += z2 << 8;
  result += z3 << 16;
  
  return result;
}

// multiply_karatsuba
//
// Multiply two 32-bit numbers by treating each as two 16-bit parts and using
// the (one-step) Karatsuba multiplication algorithm.
//
// Suppose we break up 32-bit integers a and be as follows: 
//     a = [ a_1 ] [ a_0 ] 
//     b = [ b_1 ] [ b_0 ]
//
// TODO Then we compute a * b as:
//                                   
//
// Where the outputs are the sums ofhe columns
uint32_t multiply_karatsuba( uint16_t a, uint16_t b) {
    uint16_t a_1 = (0xff00 & a) >> 8;
    uint16_t a_0 = (0x00ff & a) >>  0;
    uint16_t b_1 = (0xff00 & b) >> 8;
    uint16_t b_0 = (0x00ff & b) >>  0;
    
    uint32_t z0 = a_0 * b_0;
    uint32_t z2 = a_1 * b_1;
    uint32_t z1 = (a_1 + a_0) * (b_1 + b_0) - z0 - z2;
    
    uint32_t result = 0;
    result += z0;
    result += z1 << 8;
    result += z2 << 16;
    
    return result;
}

int main() {
    #define NUM_TESTS 3
    uint16_t tests[NUM_TESTS] = {0x0005, 0xbeef, 0xffff};
    int i, j, num_tests = NUM_TESTS;

    printf("[INFO] ------------------\n"  );
    printf("[INFO]   Addition Tests\n"   );
    printf("[INFO] ------------------\n\n");
    for(i = 0; i < num_tests; i++) {
      for( j = 0; j < num_tests; j++) {
        uint16_t a = tests[i];
        uint16_t b = tests[j];
        uint32_t r1 = add_standard(a, b);
        uint32_t r2 = add_textbook(a, b);
        printf("[INFO] add_standard (%08x, %08x) = %08x\n", a, b, r1);
        printf("[INFO] add_textbook (%08x, %08x) = %08x\n", a, b, r2);
        printf("\n");
      }
    }  

    printf("[INFO] ------------------------\n"  );
    printf("[INFO]   Multiplication Tests\n"   );
    printf("[INFO] ------------------------\n\n");
    for(i = 0; i < num_tests; i++) {
      for( j = 0; j < num_tests; j++) {
        uint16_t a = tests[i];
        uint16_t b = tests[j];
        uint32_t r1 = multiply_standard(a, b);
        uint32_t r2 = multiply_textbook(a, b);
        uint32_t r3 = multiply_karatsuba(a, b);
        printf("[INFO] multiply_standard (%08x, %08x) = %08x\n", a, b, r1);
        printf("[INFO] multiply_textbook (%08x, %08x) = %08x\n", a, b, r2);
        printf("[INFO] multiply_karatsuba(%08x, %08x) = %08x\n", a, b, r3);
        printf("\n");
      }
    }    
}
