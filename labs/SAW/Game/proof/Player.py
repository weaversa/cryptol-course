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

MAX_NAME_LENGTH = 12
SUCCESS = 170
FAILURE = 85


#######################################
# SAW Helper Functions
#######################################

def ptr_to_fresh(spec: Contract, ty: LLVMType,
                 name: str) -> Tuple[FreshVar, SetupVal]:
  var = spec.fresh_var(ty, name)
  ptr = spec.alloc(ty, points_to = var)
  return (var, ptr)

#def int_to_32_cryptol(length: int):
#  return cryptol("`{i}:[32]".format(i=length))


#######################################
# SAW Contracts
#######################################

# levelUp Contract
# uint32_t levelUp(uint32_t level)
class levelUp_Contract(Contract):
  def specification (self):
    level = self.fresh_var(i32, "level")

    self.execute_func(level)

    self.returns_f("levelUp {level}")


# initializeDefaultPlayer Contract
# uint32_t initializeDefaultPlayer(player_t* player)
class initializeDefaultPlayer_Contract(Contract):
  def specification (self):
    # Pull the struct from the bitcode
    player = self.alloc(alias_ty("struct.player_t"))

    self.execute_func(player)

    # Assert the post-condition behaviors

    # Index 0 = "name" field
    # Recall that 0x41 is ASCII for 'A'
    self.points_to(player[0], cry_f("[(i*0 + 0x41) | i <- [1..{MAX_NAME_LENGTH}]]"))

    # Index 1 is the "level" field
    self.points_to(player[1], cry_f("1 : [32]"))

    # Index 2 is the "hp" field
    self.points_to(player[2], cry_f("10 : [32]"))

    # Index 3 is the "atk" field
    self.points_to(player[3], cry_f("5 : [32]"))

    # Index 4 is the "def" field
    self.points_to(player[4], cry_f("4 : [32]"))

    # Index 5 is the "spd" field
    self.points_to(player[5], cry_f("3 : [32]"))

    self.returns(cry_f("`({SUCCESS}) : [32]"))

#######################################


#######################################
# Unit Tests
#######################################

class PlayerTests(unittest.TestCase):
  def test_levelUp(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    pwd = os.getcwd()
    bitcode_name = pwd + "/artifacts/Player.bc"
    cryptol_name = pwd + "/specs/Player.cry"

    cryptol_load_file(cryptol_name)
    module = llvm_load_module(bitcode_name)

    levelUp_result                 = llvm_verify(module, 'levelUp', levelUp_Contract())
    initializeDefaultPlayer_result = llvm_verify(module, 'initializeDefaultPlayer', initializeDefaultPlayer_Contract())
    self.assertIs(levelUp_result.is_success(), True)
    self.assertIs(initializeDefaultPlayer_result.is_success(), True)

if __name__ == "__main__":
  unittest.main()

#######################################