#include "Game.h"


uint32_t levelUp(uint32_t level)
{
  return (level + 1);
}


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


void resetInventoryItems(inventory_t* inventory)
{
  // Iterate through the inventory and set the quantity field to 0
  for (int i = 0; i < inventory->numItems; i++)
  {
    inventory->item[i].quantity = 0;
  }
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


// Simulates a complex battle where characters can counterattack as long as
// they survive to do so.
uint32_t counterBattle(player_t* player, character_t* opponent)
{
  enum battleResult result = NEUTRAL;

  // Check spd to determine who goes first
  if (player->spd >= opponent->spd)
  {
    // Player attacks first
    resolveAttack(opponent, player->atk);

    // Check if there is a counterattack
    if (opponent->hp > 0)
    {
      // Opponent counterattacks
      resolveAttack(player, opponent->atk);

      if (player->hp == 0)
      {
        result = DEFEAT_PLAYER;
      }
    }
    else
    {
      // Opponent defeated
      player->level = player->level + 1;
      result = DEFEAT_OPPONENT;
    }
  }
  else
  {
    // Opponent attacks first
    resolveAttack(player, opponent->atk);

    // Check if there is a counterattack
    if (player->hp > 0)
    {
      // Player counterattacks
      resolveAttack(opponent, player->atk);

      if (opponent->hp == 0)
      {
        result = DEFEAT_OPPONENT;
        player->level = player->level + 1;
      }
    }
    else
    {
      // Player defeated
      result = DEFEAT_PLAYER;
    }
  }

  return result;
}