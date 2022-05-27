import os
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 


# Helper functions
def ptr_to_fresh(spec: Contract, ty: LLVMType,
                 name: str) -> Tuple[FreshVar, SetupVal]:
  var = spec.fresh_var(ty, name)
  ptr = spec.alloc(ty, points_to = var)
  return (var, ptr)


# levelUp Contract
class levelUp_Contract(Contract):
  def specification (self):
    level = self.fresh_var(i32, "level")

    self.execute_func(level)

    self.returns_f("levelUp {level}")

# levelUp Unit Test
class levelUpTest(unittest.TestCase):
  def test_levelUp(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    pwd = os.getcwd()
    bitcode_name = pwd + "/artifacts/Player.bc"
    cryptol_name = pwd + "/specs/Player.cry"

    cryptol_load_file(cryptol_name)
    module = llvm_load_module(bitcode_name)

    levelUp_result = llvm_verify(module, 'levelUp', levelUp_Contract())
    self.assertIs(levelUp_result.is_success(), True)

if __name__ == "__main__":
  unittest.main()
