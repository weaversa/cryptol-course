#include "Game.h"


uint32_t initDefaultPlayer(player_t* player)
{
  // Variables
  uint8_t i = 0;
  uint32_t hp_default  = 10;
  uint32_t atk_default = 5;
  uint32_t def_default = 4;
  uint32_t spd_default = 3;

  // Initialize player according to some defaults
  for (i = 0; i < MAX_NAME_LENGTH; i++)
  {
    // For simplicity, let's use A very cool name
    // Assume ignoring the null terminator is fine too
    // Remember, this is a dummy example ;)
    player->name[i] = 'A';
  }
  player->level = 1;
  player->hp  = hp_default;
  player->atk = atk_default;
  player->def = def_default;
  player->spd = spd_default;

  return SUCCESS;
}


uint32_t initDefaultSprite(character_t* character, sprite_t* sprite)
{
  // Initialize the character to the passed pointer
  sprite->character = character;

  // Initialize the sprite frames to the default asset
  for (uint8_t i = 0; i < GAITS; i++)
  {
    for (uint8_t j = 0; j < DIRECTIONS; j++)
    {
      for (uint8_t k = 0; k < ANIMATION_STEPS; k++)
      {
        sprite->frames[i][j][k] = 0x00;
      }
    }
  }

  // Initialize sprite's default position
  sprite->xPos = 1;
  sprite->yPos = 2;

  return SUCCESS;
}


void resolveAttack(character_t* target, uint32_t atk)
{
  if ( target->def >= atk)
  {
    // The target's defense mitigates the attack
    target->hp = target->hp;
  }
  else if ( target->hp <= (atk - target->def) )
  {
    // The attack will knock out the target
    target->hp = 0;
  }
  else
  {
    // Calculate damage as normal
    target->hp = target->hp - (atk - target->def);
  }
}


uint32_t checkStats(character_t* character)
{
  // Assume failure by default
  uint32_t result = FAILURE;

  // Check the stats
  if (character->hp  <= MAX_STAT &&
      character->atk <= MAX_STAT &&
      character->def <= MAX_STAT &&
      character->spd <= MAX_STAT )
  {
    result = SUCCESS;
  }

  return result;
}