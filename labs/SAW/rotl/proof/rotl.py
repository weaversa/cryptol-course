from pathlib import Path
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 

class rotl_Contract(Contract):
  def specification(self):
    xs    = self.fresh_var(i32, "xs") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(xs, shift)

    self.returns_f("rotl {xs} {shift}")

class rotlTest(unittest.TestCase):
  def test_rotl(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    basedir = Path(__file__).absolute().parents[1] # Get absolute path to rotl/
    bcpath  = basedir/"artifacts/rotl.bc"
    crypath = basedir/"specs/rotl.cry"

    cryptol_load_file(str(crypath))
    mod = llvm_load_module(str(bcpath))
    
    rotl_result = llvm_verify(mod, 'rotl', rotl_Contract())
    self.assertIs(rotl_result.is_success(), True)

if __name__ == "__main__":
    unittest.main()
