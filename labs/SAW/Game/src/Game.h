#ifndef GAME_H
#define GAME_H

#include <stdint.h>

#define MAX_NAME_LENGTH 12
#define SUCCESS 170  // 170 = 0xAA = 10101010
#define FAILURE 85   //  85 = 0x55 = 01010101
#define MAX_STAT 100
#define SCREEN_ROWS 15
#define SCREEN_COLS 10
#define SCREEN_TILES SCREEN_ROWS*SCREEN_COLS

// Struct containing character information
typedef struct {
  uint8_t name[MAX_NAME_LENGTH];
  uint32_t level;
  uint32_t hp;
  uint32_t atk;
  uint32_t def;
  uint32_t spd;
} character_t;

typedef character_t player_t;

// Enum containing possible battle outcomes
enum battleResult{
  NEUTRAL         = 0,
  DEFEAT_PLAYER   = 1,
  DEFEAT_OPPONENT = 2
};

// Struct containing item information
typedef struct {
  uint32_t id;
  uint32_t quantity;
} item_t;

// Struct containing inventory information
typedef struct {
  //item_t* item;
  item_t item[5];
  uint32_t numItems;
} inventory_t;

// Struct containing screen information
typedef struct {
  uint8_t tiles[SCREEN_TILES];  // Holds asset ID for each screen tile
} screen_t;

// Function prototypes
uint32_t levelUp(uint32_t level);
uint32_t initDefaultPlayer(player_t* player);
uint32_t checkStats(character_t* character);
void resolveAttack(character_t* target, uint32_t atk);
void resetInventoryItems(inventory_t* inventory);
uint32_t initScreen(screen_t* screen, uint8_t assetID);
void quickBattle(player_t* player, character_t* opponent);
uint32_t counterBattle(player_t* player, character_t* opponent);


#endif