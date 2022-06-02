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
#define ASSET_TABLE_SIZE 16
#define GAITS 2            // Possible gaits to animate: walk and run
#define DIRECTIONS 4       // 2D game, 4 directions (up, down, left, right)
#define ANIMATION_STEPS 3  // 3 frames per direction (stand, left leg forward, right leg forward)


///////////////////////////////////////
// Globals
///////////////////////////////////////

const uint32_t defaultLevel = 1;
extern const uint8_t assetTable[ASSET_TABLE_SIZE];


///////////////////////////////////////
// Enums
///////////////////////////////////////

// Enum containing possible battle outcomes
enum battleResult{
  NEUTRAL         = 0,
  DEFEAT_PLAYER   = 1,
  DEFEAT_OPPONENT = 2
};


///////////////////////////////////////
// Structs
///////////////////////////////////////

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

// Struct containing item information
typedef struct {
  uint32_t id;
  uint32_t quantity;
} item_t;

// Struct containing inventory information
typedef struct {
  //item_t* item;  // Assume this points to an item_t array of unknown length
  item_t item[5];
  uint32_t numItems;
} inventory_t;

// Struct containing screen information
typedef struct {
  uint8_t tiles[SCREEN_TILES];  // Holds asset ID for each screen tile
} screen_t;

// Struct containing information on a character sprite
typedef struct {
  uint8_t frames[GAITS][DIRECTIONS][ANIMATION_STEPS];
  uint32_t xPos;  // x position relative to the screen
  uint32_t yPos;  // y position relative to the screen
} sprite_t;


///////////////////////////////////////
// Function prototypes
///////////////////////////////////////

// Function(s) with basic SAW setup
uint32_t levelUp(uint32_t level);

// Function(s) with basic struct initialization
uint32_t initDefaultPlayer(player_t* player);
uint32_t initDefaultSprite(sprite_t* sprite);

// Function(s) with preconditions and postconditions that must
// be considered in SAW contracts & unit test overrides
uint32_t checkStats(character_t* character);
void resolveAttack(character_t* target, uint32_t atk);
uint32_t selfDamage(player_t* player);
void quickBattle(player_t* player, character_t* opponent);

// Function(s) with global variable handling
uint32_t getDefaultLevel();
uint32_t initScreen(screen_t* screen, uint8_t assetID);
uint32_t setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx);

// Function(s) showing limitations with struct pointer fields
void resetInventoryItems(inventory_t* inventory);


#endif