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
MAX_STAT = 100


#######################################
# SAW Helper Functions
#######################################

def ptr_to_fresh(spec: Contract, ty: LLVMType,
                 name: str) -> Tuple[FreshVar, SetupVal]:
  var = spec.fresh_var(ty, name)
  ptr = spec.alloc(ty, points_to = var)
  return (var, ptr)

# Function that uses an existing SetupVal pointer
# Used for situations where pointer fields change during a function call
def ptr_to_existing(spec: Contract, ty: LLVMType,
                    name: str, ptr: SetupVal) -> Tuple[FreshVar, SetupVal]:
  var = spec.fresh_var(ty, name)
  spec.points_to(ptr, var)
  return (var, ptr)

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
    # Although the function uses player_t, pass character_t to SAW since
    # player_t is an alternative typedef name for character_t.
    player = self.alloc(alias_ty("struct.character_t"))

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
    #name = array
    #hp = self.fresh_var(i32, "hp")
    #defense = self.fresh_var(i32, "def")
    #target = self.alloc(struct_ty(alias_ty("struct.character_t")), points_to = struct(hp))
    (target, target_p) = ptr_to_fresh(self, alias_ty("struct.character_t"), name="target")
    atk = self.fresh_var(i32, "atk")

    # Assert the precondition that the stats are below the max stat cap
    self.precondition_f("{atk} <= `{MAX_STAT}")
    self.precondition_f("({target}).2 <= `{MAX_STAT}")
    self.precondition_f("({target}).4 <= `{MAX_STAT}")

    # Determine the preconditions based on the case parameter
    if (self.case == 1):
      # target->def >= atk
      self.precondition_f("({target}).4 >= {atk}")
    elif (self.case == 2):
      # target->hp <= (atk - target->def)
      self.precondition_f("(({target}).2 + ({target}).4) <= {atk}")
    else:
      # Assume any other case follows the formal attack calculation
      self.precondition_f("({target}).4 < {atk}")
      self.precondition_f("(({target}).2 + ({target}).4) > {atk}")
    
    # Symbolically execute the function
    self.execute_func(target_p, atk)

    # Determine the postcondition based on the case parameter
    if (self.case == 1):
      self.points_to(target_p[2], cry_f("({target}).2 : [32]"))
    elif (self.case == 2):
      self.points_to(target_p[2], cry_f("0 : [32]"))
    else:
      self.points_to(target_p[2], cry_f("resolveAttack (({target}).2) (({target}).4) {atk}"))

    self.returns(void)

#######################################


#######################################
# Unit Tests
#######################################

class PlayerTests(unittest.TestCase):
  def test_Player(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    pwd = os.getcwd()
    bitcode_name = pwd + "/artifacts/Game.bc"
    cryptol_name = pwd + "/specs/Game.cry"

    cryptol_load_file(cryptol_name)
    module = llvm_load_module(bitcode_name)

    levelUp_result                 = llvm_verify(module, 'levelUp', levelUp_Contract())
    initializeDefaultPlayer_result = llvm_verify(module, 'initializeDefaultPlayer', initializeDefaultPlayer_Contract())
    resolveAttack_case1_result     = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(1))
    resolveAttack_case2_result     = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(2))
    resolveAttack_case3_result     = llvm_verify(module, 'resolveAttack', resolveAttack_Contract(3))
    self.assertIs(levelUp_result.is_success(), True)
    self.assertIs(initializeDefaultPlayer_result.is_success(), True)
    self.assertIs(resolveAttack_case1_result.is_success(), True)
    self.assertIs(resolveAttack_case2_result.is_success(), True)
    self.assertIs(resolveAttack_case3_result.is_success(), True)

if __name__ == "__main__":
  unittest.main()

#######################################