#######################################
# Imporant Imports
#######################################

from pathlib import Path
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

# initDefaultPlayer Contract
# player_t* initDefaultPlayer()
class initDefaultPlayer_Contract(Contract):
  def specification (self):
    # No input arguments, so no need to define variables now...

    # Symbolically execute the function
    self.execute_func()

    # Allocate memory for the player_t struct
    # Note: Although the function uses player_t, pass character_t to SAW since
    #       player_t is an alternative typedef name for character_t.
    player = self.alloc(alias_ty("struct.character_t"))

    # Assert the postcondition behaviors
    self.points_to(player['name'], cry_f("repeat 0x41 : [{MAX_NAME_LENGTH}][8]"))
    self.points_to(player['level'], cry_f("1 : [32]"))
    self.points_to(player['hp'], cry_f("10 : [32]"))
    self.points_to(player['atk'], cry_f("5 : [32]"))
    self.points_to(player['def'], cry_f("4 : [32]"))
    self.points_to(player['spd'], cry_f("3 : [32]"))
    self.points_to(player['sprite'], null())

    self.returns(player)


# initDefaultSprite Contract
# uint32_t initDefaultSprite(character_t* character)
class initDefaultSprite_Contract(Contract):
  def specification (self):
    # Declare variables
    name     = self.fresh_var(array_ty(MAX_NAME_LENGTH, i8), "character.name")
    level    = self.fresh_var(i32, "character.level")
    hp       = self.fresh_var(i32, "character.hp")
    atk      = self.fresh_var(i32, "character.atk")
    defense  = self.fresh_var(i32, "character.def")
    spd      = self.fresh_var(i32, "character.spd")
    # Note: No need to allocate memory yet for the sprite_t struct since the
    #       function assumes the passed character_t struct does not have an
    #       allocated sprite. Instead, pass NULL as the sprite precondition.
    character_p = self.alloc( alias_ty("struct.character_t")
                            , points_to = struct( name
                                                , level
                                                , hp
                                                , atk
                                                , defense
                                                , spd
                                                , null() ))

    # Symbolically execute the function
    self.execute_func(character_p)

    # Allocate memory for the new sprite_t struct and assert its postconditions
    sprite_p = self.alloc( alias_ty("struct.sprite_t")
                         , points_to = struct( cry_f("zero : [{GAITS}][{DIRECTIONS}][{ANIMATION_STEPS}][8]")
                                             , cry_f("1 : [32]")
                                             , cry_f("2 : [32]") ))

    # Assert the postcondition for character
    self.points_to(character_p, struct( name
                                      , level
                                      , hp
                                      , atk
                                      , defense
                                      , spd
                                      , sprite_p ))

    self.returns_f("`({SUCCESS}) : [32]")


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
    atk                = self.fresh_var(i32, "atk")

    # Assert the precondition that the stats are below the max stat cap
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
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("{target}.4 < {atk}")
      self.precondition_f("({target}.2 + {target}.4) > {atk}")
    
    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Determine the postcondition based on the case parameter
    if (self.case == 1):
      self.points_to(target_p['hp'], cry_f("{target}.2 : [32]"))
    elif (self.case == 2):
      self.points_to(target_p['hp'], cry_f("0 : [32]"))
    else:
      self.points_to(target_p['hp'], cry_f("resolveAttack ({target}.2) ({target}.4) {atk}"))

    self.returns(void)


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


#######################################
# Unit Tests
#######################################

class GameTests(unittest.TestCase):
  def test_Game(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    basedir = Path(__file__).absolute().parents[1] # Get absolute path to Game/
    bcpath  = basedir/"artifacts/Game.bc"
    crypath = basedir/"specs/Game.cry"

    cryptol_load_file(str(crypath))
    module = llvm_load_module(str(bcpath))

    # Set up verification overrides
    initDefaultPlayer_result   = llvm_verify(module, 'initDefaultPlayer', initDefaultPlayer_Contract())
    initDefaultSprite_result   = llvm_verify(module, 'initDefaultSprite', initDefaultSprite_Contract())
    resolveAttack_case1_result = llvm_verify(module, 'resolveAttack'    , resolveAttack_Contract(1)   )
    resolveAttack_case2_result = llvm_verify(module, 'resolveAttack'    , resolveAttack_Contract(2)   )
    resolveAttack_case3_result = llvm_verify(module, 'resolveAttack'    , resolveAttack_Contract(3)   )
    checkStats_pass_result     = llvm_verify(module, 'checkStats'       , checkStats_Contract(True)   )
    checkStats_fail_result     = llvm_verify(module, 'checkStats'       , checkStats_Contract(False)  )

    # Assert the overrides are successful
    self.assertIs(initDefaultPlayer_result.is_success()  , True)
    self.assertIs(initDefaultSprite_result.is_success()  , True)
    self.assertIs(resolveAttack_case1_result.is_success(), True)
    self.assertIs(resolveAttack_case2_result.is_success(), True)
    self.assertIs(resolveAttack_case3_result.is_success(), True)
    self.assertIs(checkStats_pass_result.is_success()    , True)
    self.assertIs(checkStats_fail_result.is_success()    , True)

if __name__ == "__main__":
  unittest.main()
