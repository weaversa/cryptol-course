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
# uint32_t initDefaultPlayer(player_t* player)
class initDefaultPlayer_Contract(Contract):
  def specification (self):
    # TODO: Declare variables


    # TODO: Assert preconditions


    # TODO: Symbolically execute the function


    # TODO: Assert postconditions


    # TODO: Remove the following line when you have something!
    pass


# initDefaultSprite Contract
# uint32_t initDefaultSprite(character_t* character, sprite_t* sprite)
class initDefaultSprite_Contract(Contract):
  def specification (self):
    # TODO: Declare variables


    # TODO: Assert preconditions


    # TODO: Symbolically execute the function


    # TODO: Assert postconditions


    # TODO: Remove the following line when you have something!
    pass


# resolveAttack Contract
# void resolveAttack(character_t* target, uint32_t atk)
class resolveAttack_Contract(Contract):
  def specification (self):
    # TODO: Declare variables


    # TODO: Assert preconditions

    
    # TODO: Symbolically execute the function


    # TODO: Assert postconditions


    # TODO: Remove the following line when you have something!
    pass


# checkStats Contract
# uint32_t checkStats(character_t* character)
class checkStats_Contract(Contract):
  def specification (self):
    # TODO: Declare variables


    # TODO: Assert preconditions

    
    # TODO: Symbolically execute the function


    # TODO: Assert postconditions


    # TODO: Remove the following line when you have something!
    pass


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

    # TODO: Set up verification overrides


    # TODO: Assert the overrides are successful


if __name__ == "__main__":
  unittest.main()
