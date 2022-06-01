import os
import unittest
from saw_client             import *
from saw_client.crucible    import * 
from saw_client.llvm        import * 
from saw_client.proofscript import *
from saw_client.llvm_type   import * 

class RCS_Contract(Contract):
  def specification(self):
    xs    = self.fresh_var(i32, "xs") 
    shift = self.fresh_var(i32, "shift")
    
    self.execute_func(xs, shift)

    self.returns_f("RCS {xs} {shift}")

class RCSTest(unittest.TestCase):
  def test_RCS(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    pwd = os.getcwd()
    bcname  = pwd + "/../src/rcs3.bc"
    cryname = pwd + "/spec/rcs.cry"

    cryptol_load_file(cryname)
    mod = llvm_load_module(bcname)
    
    RCS_result = llvm_verify(mod, 'RCS', RCS_Contract())
    self.assertIs(RCS_result.is_success(), True)

if __name__ == "__main__":
    unittest.main()
