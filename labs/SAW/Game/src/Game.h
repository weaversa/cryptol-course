#ifndef GAME_H
#define GAME_H

#include <stdint.h>

#define MAX_NAME_LENGTH 12
#define SUCCESS 170  // 170 = 0xAA = 10101010
#define FAILURE 85   //  85 = 0x55 = 01010101
#define MAX_STAT 100


typedef struct {
  uint8_t name[MAX_NAME_LENGTH];
  uint32_t level;
  uint32_t hp;
  uint32_t atk;
  uint32_t def;
  uint32_t spd;
} character_t;

typedef character_t player_t;

typedef struct {
  uint32_t id;
  uint32_t quantity;
} item_t;

typedef struct {
  item_t* item;
  uint32_t numItems;
} inventory_t;

// Function prototypes
uint32_t levelUp(uint32_t level);
uint32_t initializeDefaultPlayer(player_t* player);
void resolveAttack(character_t* target, uint32_t atk);
void battle(player_t* player, character_t* enemy);
void resetInventoryItems(inventory_t* inventory);


#endif