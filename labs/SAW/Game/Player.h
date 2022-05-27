#ifndef PLAYER_H
#define PLAYER_H

#include <stdint.h>

#define MAX_NAME_LENGTH 12
#define SUCCESS 170  // 170 = 0xAA = 10101010
#define FAILURE 85   //  85 = 0x55 = 01010101

typedef struct {
  uint8_t name[MAX_NAME_LENGTH];
  uint32_t level;
  uint32_t hp;
  uint32_t atk;
  uint32_t def;
  uint32_t spd;
} player_t;


// Function prototypes
uint32_t initializeDefaultPlayer(player_t* player);

#endif