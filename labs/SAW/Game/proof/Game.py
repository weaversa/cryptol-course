# TODO
# - Try the alloc_pointsto_buffer example & see its limitations
#   --> See if possible to keep pointer field
# - Aligned
# - Make a worksheet & solution for the SAW examples
# - Update SAW.md to discuss the topics covered in this SAW script
#   --> Link to the GaloicInc/saw-script and GaloisInc/saw-demos examples for more Python references
#       https://github.com/GaloisInc/saw-script/tree/master/saw-remote-api/python/tests/saw
#       https://github.com/GaloisInc/saw-demos/tree/master/demos/signal-protocol
#   --> Mention importance of "-g" flag for debug symbols
#   --> Discuss reasons between self.alloc (only want pointer) and
#       ptr_to_fresh (want to set initial contents or reference in postcondition)
#   --> Mention reason for preconditions (i.e. checkStats caller vs callee)
#       Removing the preconditons in resolveAttack leads to integer overflow
#   --> Mention in some SAW configs (i.e. llvm), nested defines may be an issue (i.e. initScreen)
#   --> Mention in some SAW configs (i.e. llvm), aliasing/non-disjoint memory regions may be an issue (i.e. selfDamage)
#   --> Discuss the issues associated with inventory_t's item_t pointer
#   --> Include a "Troubleshooting/What SAW Says & Means" section
# - Reorder the functions to match the topic discussion


#######################################
# Imporant Imports
#######################################

import os
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 


#######################################
# Defines and Constants
#######################################

MAX_NAME_LENGTH  = 12
SUCCESS          = 170
FAILURE          = 85
MAX_STAT         = 100
SCREEN_ROWS      = 15
SCREEN_COLS      = 10
SCREEN_TILES     = SCREEN_ROWS * SCREEN_COLS
NEUTRAL          = 0
DEFEAT_PLAYER    = 1
DEFEAT_OPPONENT  = 2
ASSET_TABLE_SIZE = 16
GAITS            = 2
DIRECTIONS       = 4
ANIMATION_STEPS  = 3

#######################################
# SAW Helper Functions
#######################################

def ptr_to_fresh(spec: Contract, ty: LLVMType,
                 name: str) -> Tuple[FreshVar, SetupVal]:
  var = spec.fresh_var(ty, name)
  ptr = spec.alloc(ty, points_to = var)
  return (var, ptr)


#######################################
# SAW Contracts
#######################################

# levelUp Contract
# uint32_t levelUp(uint32_t level)
class levelUp_Contract(Contract):
  def specification (self):
    # Declare level variable
    level = self.fresh_var(i32, "level")

    # Symbolically execute the function
    self.execute_func(level)

    # Assert the function's return behavior
    self.returns_f("levelUp {level}")


# initDefaultPlayer Contract
# uint32_t initDefaultPlayer(player_t* player)
class initDefaultPlayer_Contract(Contract):
  def specification (self):
    # Pull the struct from the bitcode
    # Although the function uses player_t, pass character_t to SAW since
    # player_t is an alternative typedef name for character_t.
    player = self.alloc(alias_ty("struct.character_t"))

    # Symbolically execute the function
    self.execute_func(player)

    # Assert the postcondition behaviors

    # Option 1: Assert one field at a time via points_to
    self.points_to(player['name'], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    self.points_to(player['level'], cry_f("1 : [32]"))
    self.points_to(player['hp'], cry_f("10 : [32]"))
    self.points_to(player['atk'], cry_f("5 : [32]"))
    self.points_to(player['def'], cry_f("4 : [32]"))
    self.points_to(player['spd'], cry_f("3 : [32]"))
    # Note: If bitcode isn't compiled with debug symbols enabled (no "-g" flag)
    #       then use the field indices instead.
    # self.points_to(player[0], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    # self.points_to(player[1], cry_f("1 : [32]"))
    # self.points_to(player[2], cry_f("10 : [32]"))
    # self.points_to(player[3], cry_f("5 : [32]"))
    # self.points_to(player[4], cry_f("4 : [32]"))
    # self.points_to(player[5], cry_f("3 : [32]"))

    # Option 2: Assert all of the fields to a tuple
    # self.points_to(player, cry_f("( repeat 0x41 : [{MAX_NAME_LENGTH}][8], 1 : [32], 10 : [32], 5 : [32], 4 : [32], 3 : [32] )"))

    self.returns(cry_f("`({SUCCESS}) : [32]"))


# initDefaultSprite Contract
# uint32_t initDefaultSprite(sprite_t* sprite)
class initDefaultSprite_Contract(Contract):
  def specification (self):
    # Declare variables
    ty = array_ty(GAITS, array_ty(DIRECTIONS, array_ty(ANIMATION_STEPS, i8)))
    frames = self.fresh_var(ty, "sprite.frames")
    xPos = self.fresh_var(i32, "sprite.xPos")
    yPos = self.fresh_var(i32, "sprite.yPos")
    sprite_p = self.alloc(alias_ty("struct.sprite_t"), points_to = struct(frames, xPos, yPos))

    # Symbolically execute the function
    self.execute_func(sprite_p)

    # Assert postconditions
    self.points_to(sprite_p, struct(cry_f("""(zero, 1 , 2)
      : ([{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8], [32], [32])""")))                              
                                           
    self.returns_f("`({SUCCESS}) : [32]")


# checkStats Contract
# uint32_t checkStats(character_t* character)
class checkStats_Contract(Contract):
  def __init__(self, shouldPass : bool):
    super().__init__()
    self.shouldPass = shouldPass
    # There are 2 possible cases for checkStats
    # Case 1 (shouldPass == True): All of the stats are below the MAX_STAT cap
    # Case 2 (shouldPass == False): At least one stat exceeds the MAX_STAT cap

  def specification (self):
    # Declare variables
    (character, character_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="character")

    # Assert preconditions
    if (self.shouldPass):
      # Set up the preconditions to assure all stats are below the MAX_STAT cap
      self.precondition_f("{character}.2 <= `{MAX_STAT}")
      self.precondition_f("{character}.3 <= `{MAX_STAT}")
      self.precondition_f("{character}.4 <= `{MAX_STAT}")
      self.precondition_f("{character}.5 <= `{MAX_STAT}")
    else:
      # Note: Must meet at least one of the following preconditions to result
      #       in a failure (feel free to comment out as many as you want)
      self.precondition_f("{character}.2 > `{MAX_STAT}")
      self.precondition_f("{character}.3 > `{MAX_STAT}")
      self.precondition_f("{character}.4 > `{MAX_STAT}")
      self.precondition_f("{character}.5 > `{MAX_STAT}")

    # Symbolically execute the function
    self.execute_func(character_p)

    # Assert the postcondition behavior
    if (self.shouldPass):
      self.returns(cry_f("`({SUCCESS}) : [32]"))
    else:
      self.returns(cry_f("`({FAILURE}) : [32]"))


# resolveAttack Contract
# void resolveAttack(character_t* target, uint32_t atk)
class resolveAttack_Contract(Contract):
  def __init__(self, case : int):
    super().__init__()
    self.case = case
    # There are 3 possible cases for resolveAttack
    #   Case 1: Attack mitigated
    #   Case 2: Immediate KO
    #   Case 3: Regular attack
    # Each case results in a different function behavior for calculating the
    # target's remaining HP. While we could make 3 separate contracts to handle
    # all of the possible cases, we can pass a parameter to the contract, which
    # identifies what preconditions and postconditions to set.

  def specification (self):
    # Declare variables
    (target, target_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="target")
    atk = self.fresh_var(i32, "atk")

    # Assert the precondition that the stats are below the max stat cap
    # Pop Quiz: Why do we need these preconditions?
    self.precondition_f("{atk} <= `{MAX_STAT}")
    self.precondition_f("{target}.2 <= `{MAX_STAT}")
    self.precondition_f("{target}.4 <= `{MAX_STAT}")

    # Determine the preconditions based on the case parameter
    if (self.case == 1):
      # target->def >= atk
      self.precondition_f("{target}.4 >= {atk}")
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("({target}.2 + {target}.4) <= {atk}")
      # Pop Quiz: Are the following preconditions better for case 2?
      # self.precondition_f("{target}.4 < {atk}")
      # self.precondition_f("{target}.2 <= ({atk} - {target}.4)")
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("{target}.4 < {atk}")
      self.precondition_f("({target}.2 + {target}.4) > {atk}")
    
    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Determine the postcondition based on the case parameter
    if (self.case == 1):
      self.points_to(target_p['hp'], cry_f("{target}.2 : [32]"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(target_p[2], cry_f("{target}.2 : [32]"))
    elif (self.case == 2):
      self.points_to(target_p['hp'], cry_f("0 : [32]"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(target_p[2], cry_f("0 : [32]"))
    else:
      self.points_to(target_p['hp'], cry_f("resolveAttack ({target}.2) ({target}.4) {atk}"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(target_p[2], cry_f("resolveAttack ({target}.2) ({target}.4) {atk}"))

    self.returns(void)


# selfDamage Contract
# uint32_t selfDamage(player_t* player)
class selfDamage_Contract(Contract):
  def __init__(self, case : int):
    super().__init__()
    self.case = case
    # There are 3 possible cases for resolveAttack
    #   Case 1: Attack mitigated
    #   Case 2: Immediate KO
    #   Case 3: Regular attack
    # Each case results in a different function behavior for calculating the
    # player's remaining HP. While we could make 3 separate contracts to handle
    # all of the possible cases, we can pass a parameter to the contract, which
    # identifies what preconditions and postconditions to set.

  def specification (self):
    # Declare variables
    (player, player_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="player")

    # Assert the precondition that the player's HP is positive
    # Why? Game logic assumes you can't damage yourself if you're already KO'd!
    self.precondition_f("{player}.2 > 0")

    # Assert the precondition that character stats are below the max stat cap
    # Pop Quiz: Explain why the proof fails when the following preconditions
    #           are commented out.
    self.precondition_f("{player}.2   <= `{MAX_STAT}")
    self.precondition_f("{player}.3   <= `{MAX_STAT}")
    self.precondition_f("{player}.4   <= `{MAX_STAT}")
    self.precondition_f("{player}.5   <= `{MAX_STAT}")

    # Determine the preconditions based on the case parameter
    if (self.case == 1):
      # player->def >= player->atk
      self.precondition_f("{player}.4 >= {player}.3")
    elif (self.case == 2):
      # player->hp <= (player->atk - player->def)
      self.precondition_f("({player}.2 + {player}.4) <= {player}.3")
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("{player}.4 < {player}.3")
      self.precondition_f("({player}.2 + {player}.4) > {player}.3")

    # Symbolically execute the function
    self.execute_func(player_p, player_p)

    # Assert the postcondition
    if (self.case == 1):
      self.points_to(player_p['hp'], cry_f("{player}.2 : [32]"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(player_p[2], cry_f("{player}.2 : [32]"))
      self.returns(cry_f("`({NEUTRAL}) : [32]"))
    elif (self.case == 2):
      self.points_to(player_p['hp'], cry_f("0 : [32]"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(player_p[2], cry_f("0 : [32]"))
      self.returns(cry_f("`({DEFEAT_PLAYER}) : [32]"))
    else:
      self.points_to(player_p['hp'], cry_f("resolveAttack ({player}.2) ({player}.4) {player}.3"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(player_p[2], cry_f("resolveAttack ({player}.2) ({player}.4) {player}.3"))
      self.returns(cry_f("`({NEUTRAL}) : [32]"))


# quickBattle Contract
# void quickBattle(player_t* player, character_t* opponent)
class quickBattle_Contract(Contract):
  def specification (self):
    # Declare variables
    (player, player_p)     = ptr_to_fresh(self, alias_ty("struct.character_t"), name="player")
    (opponent, opponent_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="opponent")
    # Pop Quiz: Why does allocating the pointers in the following way yield an
    #           error?
    #player = self.alloc(alias_ty("struct.character_t"))
    #opponent = self.alloc(alias_ty("struct.character_t"))

    # Assert the precondition that both HPs are greater than 0
    # Why? Game logic assumes you can't attack if you're already KO'd!
    self.precondition_f("{player}.2   > 0")
    self.precondition_f("{opponent}.2 > 0")

    # Assert the precondition that character stats are below the max stat cap
    # Pop Quiz: Explain why the proof fails when the following preconditions
    #           are commented out.
    self.precondition_f("{player}.2   <= `{MAX_STAT}")
    self.precondition_f("{player}.3   <= `{MAX_STAT}")
    self.precondition_f("{player}.4   <= `{MAX_STAT}")
    self.precondition_f("{player}.5   <= `{MAX_STAT}")
    self.precondition_f("{opponent}.2 <= `{MAX_STAT}")
    self.precondition_f("{opponent}.3 <= `{MAX_STAT}")
    self.precondition_f("{opponent}.4 <= `{MAX_STAT}")
    self.precondition_f("{opponent}.5 <= `{MAX_STAT}")

    # Symbolically execute the function
    self.execute_func(player_p, opponent_p)

    # Assert the postcondition
    self.returns(void)


# getDefaultLevel Contract
# uint32_t getDefaultLevel()
class getDefaultLevel_Contract(Contract):
  def specification (self):
    # Initialize the defaultLevel global variable
    defaultLevel_init = global_initializer("defaultLevel")
    self.points_to(global_var("defaultLevel"), defaultLevel_init)

    # Symbolically execute the function
    self.execute_func()

    # Assert the function's return behavior
    self.returns(defaultLevel_init)


# initScreen Contract
# uint32_t initScreen(screen_t* screen, uint8_t assetID)
class initScreen_Contract(Contract):
  def specification (self):
    # Declare variables
    screen = self.alloc(alias_ty("struct.screen_t"))
    assetID = self.fresh_var(i8, "assetID")

    # Symbolically execute the function
    self.execute_func(screen, assetID)

    # Assert the postcondition
    self.points_to(screen['tiles'], cry_f("repeat {assetID} : [{SCREEN_TILES}][8]"))
    # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
    # self.points_to(screen[0], cry_f("repeat {assetID} : [{SCREEN_TILES}][8]"))

    self.returns(cry_f("`({SUCCESS}) : [32]"))


# setScreenTile
# uint32_t setScreenTile(screen_t* screen, uint32_t screenIdx, uint32_t tableIdx)
class setScreenTile_Contract(Contract):
  def __init__(self, shouldPass : bool):
    super().__init__()
    self.shouldPass = shouldPass
    # There are 2 possible cases for setScreenTile
    # Case 1 (shouldPass == True): Both screenIdx and tableIdx are below their
    #                              limits, SCREEN_TILES and ASSET_TABLE_SIZE,
    #                              respectively.
    # Case 2 (shouldPass == False): At least one index exceeds its limits.

  def specification (self):
    # Declare variables
    (screen, screen_p) = ptr_to_fresh(self, alias_ty("struct.screen_t"), name="screen")
    screenIdx = self.fresh_var(i32, "screenIdx")
    tableIdx  = self.fresh_var(i32, "tableIdx")
    # Pop Quiz: Why can't we just declare and pass the screen pointer?
    # screen_p = self.alloc(alias_ty("struct.screen_t"))

    # Initialize Game.h's assetTable according to Assets.c
    #   Note: The contents of assetTable from Assets.c was copied into Game.cry
    # Required because the global variable in Game.h is declared as an extern,
    # and the current way Game.bc is compiled does not include Assets.c in the
    # Makefile.
    self.points_to(global_var("assetTable"), cry_f("assetTable"))

    # Assert preconditions depending on the Contract parameter
    if (self.shouldPass):
      self.precondition_f("{screenIdx} < {SCREEN_TILES}")
      self.precondition_f("{tableIdx}  < {ASSET_TABLE_SIZE}")
    else:
      # Note: Only one of the following preconditions is needed
      self.precondition_f("{screenIdx} >= {SCREEN_TILES}")
      self.precondition_f("{tableIdx}  >= {ASSET_TABLE_SIZE}")
    
    # Symbolically execute the function
    self.execute_func(screen_p, screenIdx, tableIdx)

    # Since we just want to check one index, let's have our screen pointer
    # point to a new screen_t variable.
    screen_post = self.fresh_var(alias_ty("struct.screen_t"), "screen_post")

    # Assert that the original screen pointer now points to the new screen_t
    # variable. This will allow us to reference screen_post in Cryptol for our
    # later postconditions.
    self.points_to(screen_p, screen_post)

    # Assert postconditions depending on the Contract parameter
    if (self.shouldPass):
      self.postcondition_f("({screen_post}@{screenIdx}) == assetTable@{tableIdx}")
      #self.points_to(screen['tiles'][cry_f("{screenIdx}")], cry_f("assetTable@{tableIdx}"))
      self.returns_f("`({SUCCESS}) : [32]")
    else:
      self.returns_f("`({FAILURE}) : [32]")


# resetInventoryItems Contract
# void resetInventoryItems(inventory_t* inventory)
class resetInventoryItems_Contract(Contract):
  def __init__(self, numItems : int):
    super().__init__()
    self.numItems = numItems
    # Note: The inventory_t struct defines its item field as a item_t pointer.
    #       Instead of declaring a fixed array size, the pointer enables for a
    #       variable size depending on the memory allocation configured by its
    #       caller.
    #       While this is valid C syntax, SAW and Cryptol require the known
    #       size ahead of time. Here, our contract takes the numItems parameter
    #       to declare a fixed array size.

  def specification (self):
    # Declare variables
    # Note: The setup here does not use item_p for the proof. However, item_p
    #       is included to show errors that can be encountered with the
    #       inventory_t struct.
    (item, item_p) = ptr_to_fresh(self, array_ty(self.numItems, alias_ty("struct.item_t")), name="item")
    inventory_p = self.alloc(alias_ty("struct.inventory_t"), points_to = struct(item, cry_f("{self.numItems} : [32]")))
    """
    If inventory_t is defined as:
      typedef struct {
        item_t* item;
        uint32_t numItems;
      } inventory_t;

    Attempt 1:
    ==========
    Passing item as so:
      inventory_p = self.alloc(alias_ty("struct.inventory_t"), points_to = struct(item, cry_f("{self.numItems} : [32]")))

    yields the following error:
      ⚠️  Failed to verify: lemma_resetInventoryItems_Contract (defined at proof/Game.py:190):
      error: types not memory-compatible:
      { %struct.item_t*, i32 }
      { [5 x { i32, i32 }], i32 }


              stdout:

    Attempt 2:
    ==========
    Passing item_p as so:
      inventory_p = self.alloc(alias_ty("struct.inventory_t"), points_to = struct(item_p, cry_f("{self.numItems} : [32]")))
  
    yields the following error:
      ⚠️  Failed to verify: lemma_resetInventoryItems_Contract (defined at proof/Game.py:190):
      error: typeOfSetupValue: llvm_elem requires pointer to struct or array, found %struct.item_t**
              stdout:

    Considering both of these verification setup attempts, we can see that
    defining inventory_t with an item_t pointer is tough for SAW to setup and.
    prove. Consequently, it is better to use fixed array lengths for structs!
    """

    # Symbolically execute the function
    self.execute_func(inventory_p)

    # Assert the postconditions
    for i in range(self.numItems):
      self.points_to(inventory_p['item'][i][1], cry_f("0 : [32]"))
      # If bitcode is compiled without debug symbols enabled (no "-g" flag), use:
      # self.points_to(inventory_p[0][i][1], cry_f("0 : [32]"))
      # Note: Even though debug symbols is enabled, SAW cannot resolve the
      #       "quantity" field, which is why we still use a 1 above.

    self.returns(void)


#######################################


#######################################
# Unit Tests
#######################################

class GameTests(unittest.TestCase):
  def test_Game(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    pwd = os.getcwd()
    bitcode_name = pwd + "/artifacts/Game.bc"
    cryptol_name = pwd + "/specs/Game.cry"

    cryptol_load_file(cryptol_name)
    module = llvm_load_module(bitcode_name)

    # Override(s) associated with basic SAW setup
    levelUp_result             = llvm_verify(module, 'levelUp', levelUp_Contract())

    # Override(s) associated with basic struct initialization
    initDefaultPlayer_result   = llvm_verify(module, 'initDefaultPlayer', initDefaultPlayer_Contract())
    initDefaultSprite_result   = llvm_verify(module, 'initDefaultSprite', initDefaultSprite_Contract())

    # Overrides(s) associated with preconditions and postconditions that must
    # be considered in SAW contracts & unit test overrides
    checkStats_pass_result     = llvm_verify(module, 'checkStats', checkStats_Contract(True))
    checkStats_fail_result     = llvm_verify(module, 'checkStats', checkStats_Contract(False))
    resolveAttack_case1_result = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(1))
    resolveAttack_case2_result = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(2))
    resolveAttack_case3_result = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(3))
    selfDamage_case1_result    = llvm_verify(module, 'selfDamage', selfDamage_Contract(1), lemmas=[resolveAttack_case1_result, resolveAttack_case2_result, resolveAttack_case3_result])
    selfDamage_case2_result    = llvm_verify(module, 'selfDamage', selfDamage_Contract(2), lemmas=[resolveAttack_case1_result, resolveAttack_case2_result, resolveAttack_case3_result])
    selfDamage_case3_result    = llvm_verify(module, 'selfDamage', selfDamage_Contract(3), lemmas=[resolveAttack_case1_result, resolveAttack_case2_result, resolveAttack_case3_result])
    quickBattle_result         = llvm_verify(module, 'quickBattle', quickBattle_Contract(), lemmas=[resolveAttack_case1_result, resolveAttack_case2_result, resolveAttack_case3_result])

    # Override(s) associated with global variable handling
    getDefaultLevel_result     = llvm_verify(module, 'getDefaultLevel', getDefaultLevel_Contract())
    initScreen_result          = llvm_verify(module, 'initScreen', initScreen_Contract())
    setScreenTile_pass_result  = llvm_verify(module, 'setScreenTile', setScreenTile_Contract(True))
    setScreenTile_fail_result  = llvm_verify(module, 'setScreenTile', setScreenTile_Contract(False))

    # Override(s) showing limitations with struct pointer fields
    resetInventoryItems_result = llvm_verify(module, 'resetInventoryItems', resetInventoryItems_Contract(5))

    # Assert the overrides are successful
    self.assertIs(levelUp_result.is_success(), True)
    self.assertIs(initDefaultPlayer_result.is_success(), True)
    self.assertIs(initDefaultSprite_result.is_success(), True)
    self.assertIs(checkStats_pass_result.is_success(), True)
    self.assertIs(checkStats_fail_result.is_success(), True)
    self.assertIs(resolveAttack_case1_result.is_success(), True)
    self.assertIs(resolveAttack_case2_result.is_success(), True)
    self.assertIs(resolveAttack_case3_result.is_success(), True)
    self.assertIs(selfDamage_case1_result.is_success(), True)
    self.assertIs(selfDamage_case2_result.is_success(), True)
    self.assertIs(selfDamage_case3_result.is_success(), True)
    self.assertIs(quickBattle_result.is_success(), True)
    self.assertIs(getDefaultLevel_result.is_success(), True)
    self.assertIs(initScreen_result.is_success(), True)
    self.assertIs(setScreenTile_pass_result.is_success(), True)
    self.assertIs(setScreenTile_fail_result.is_success(), True)    
    self.assertIs(resetInventoryItems_result.is_success(), True)


if __name__ == "__main__":
  unittest.main()

#######################################
