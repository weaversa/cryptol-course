#include "Game.h"


///////////////////////////////////////
// Function(s) with basic SAW setup
///////////////////////////////////////

uint32_t levelUp(uint32_t level)
{
  return (level + 1);
}


///////////////////////////////////////
// Function(s) with basic struct initialization
///////////////////////////////////////

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


///////////////////////////////////////
// Function(s) with preconditions and postconditions that must
// be considered in SAW contracts & unit test overrides
///////////////////////////////////////

// Checks that the character stats are below MAX_STAT
// Note: MUST be called before running any function that uses character stats!
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


uint32_t selfDamage(player_t* player)
{
  enum battleResult result = NEUTRAL;

  // Player attacks itself (perhaps due to status condition)
  resolveAttack(player, player->atk);
  // Note: If SAW ever gives a "Memory not disjoint" error, then what SAW
  //       actually means is that inputs to an overridden function cannot
  //       overlap in memory. Given that player->atk is a field within player,
  //       the inputs overlap.
  // Also consider the following way to have the player damage itself:
  //   quickBattle(player, player);
  // This also contains inputs that overlap in memory (i.e. not disjoint).
  // Bear in mind that both function calls in this example will not result in
  // the Memory Disjoint error for the Python API. However, they will likely
  // yield the same error if not using the Python API (i.e. llvm.saw).

  if (player->hp <= 0)
  {
    // Player's self damage results in a knockout
    result = DEFEAT_PLAYER;
  }

  return result;
}


// Simulates a simple battle where only the faster character attacks.
void quickBattle(player_t* player, character_t* opponent)
{
  if (player->spd >= opponent->spd)
  {
    resolveAttack(opponent, player->atk);
  }
  else
  {
    resolveAttack(player, opponent->atk);
  }
}


///////////////////////////////////////
// Function(s) with global variable handling
///////////////////////////////////////

uint32_t getDefaultLevel()
{
  return defaultLevel;
}


uint32_t initScreen(screen_t* screen, uint8_t assetID)
{
  // Iterate through the screen tiles and set their asset ID
  for (int i = 0; i < SCREEN_TILES; i++)
  {
    screen->tiles[i] = assetID;
  }

  return SUCCESS;
}


// Sets a tile displayed on the screen to an particular assetID held in the
// global assetTable
uint32_t setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx)
{
  // Initialize return status to FAILURE
  uint32_t result = FAILURE;

  // Check for valid bounds
  if (screenIdx < SCREEN_TILES && tableIdx < ASSET_TABLE_SIZE)
  {
    screen->tiles[screenIdx] = assetTable[tableIdx];
    result = SUCCESS;
  }

  return result;
}



///////////////////////////////////////
// Function(s) showing limitations with struct pointer fields
///////////////////////////////////////

void resetInventoryItems(inventory_t* inventory)
{
  // Iterate through the inventory and set the quantity field to 0
  for (int i = 0; i < inventory->numItems; i++)
  {
    inventory->item[i].quantity = 0;
  }
}
