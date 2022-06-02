#ifndef GAME_H
#define GAME_H

#include <stdint.h>


// Define status values that provide a hamming distance of 5
#define SUCCESS 170  // 170 = 0xAA = 10101010
#define FAILURE 85   //  85 = 0x55 = 01010101

#define MAX_NAME_LENGTH 12
#define MAX_STAT 100
#define GAITS 2            // Possible gaits to animate: walk and run
#define DIRECTIONS 4       // 2D game, 4 directions (up, down, left, right)
#define ANIMATION_STEPS 3  // 3 frames per direction (stand, left leg forward, right leg forward)


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

// Struct containing information on a character sprite
typedef struct {
  character_t* character;
  uint8_t frames[GAITS][DIRECTIONS][ANIMATION_STEPS];
  uint32_t xPos;  // x position relative to the screen
  uint32_t yPos;  // y position relative to the screen
} sprite_t;


///////////////////////////////////////
// Function prototypes
///////////////////////////////////////

uint32_t initDefaultPlayer(player_t* player);
uint32_t initDefaultSprite(character_t* character, sprite_t* sprite);
uint32_t checkStats(character_t* character);
void resolveAttack(character_t* target, uint32_t atk);


#endif
