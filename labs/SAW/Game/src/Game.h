#ifndef GAME_H
#define GAME_H

#include <stdint.h>


// Status values that provide a hamming distance of 8
#define SUCCESS 170         // 170 = 0xAA = 10101010
#define FAILURE 85          //  85 = 0x55 = 01010101

#define MAX_NAME_LENGTH 12  // Maximum number of bytes in a character name
#define MAX_STAT 100        // Inclusive upper bound limit on character stats

#define GAITS 2             // Possible gaits to animate: walk and run
#define DIRECTIONS 4        // 2D game, 4 directions (up, down, left, right)
#define ANIMATION_STEPS 3   // 3 frames per direction (stand, left leg forward, right leg forward)


///////////////////////////////////////
// Structs
///////////////////////////////////////

// Contains information related to character sprites
typedef struct {
  uint8_t frames[GAITS][DIRECTIONS][ANIMATION_STEPS];
  uint32_t xPos;  // x position relative to the screen
  uint32_t yPos;  // y position relative to the screen
} sprite_t;


// Contains information about in-game characters
typedef struct {
  uint8_t name[MAX_NAME_LENGTH];
  uint32_t level;
  uint32_t hp;
  uint32_t atk;
  uint32_t def;
  uint32_t spd;
  sprite_t* sprite;
} character_t;

typedef character_t player_t;


///////////////////////////////////////
// Function prototypes
///////////////////////////////////////

/**
  Initializes a player variable based on default parameters.
  \param None
  \return player_t* player - Pointer to an allocated player variable.
**/
player_t* initDefaultPlayer();

/**
  Initializes a sprite variable based on default parameters and ties the sprite
  to the passed character reference. Assumes sprite initialization can only
  once for a character.
  \param character_t* character - Pointer to a character variable that the
                                  the sprite should be tied to.
  \return SUCCESS if a sprite is initialized for character, or FAILURE if
          sprite initialization fails.
**/
uint32_t initDefaultSprite(character_t* character);

/**
  Resolves a target's hp stat after an attack.
  \param character_t* target - Defender during the attack.
  \param uint32_t atk - Attacker's atk stat.
  \return None.
**/
void resolveAttack(character_t* target, uint32_t atk);

/**
  Checks whether the referenced character's stats are at or below the MAX_STAT.
  \param character_t* character - Pointer to the character in question.
  \return SUCCESS when all stats are <= MAX_STAT, or FAILURE otherwise.
**/
uint32_t checkStats(character_t* character);


#endif
