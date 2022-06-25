from pathlib import Path
from unittest import TestCase, main

from saw_client import LogResults, connect, llvm_load_module, llvm_verify, view
from saw_client.crucible import Contract
from saw_client.llvm import i64

class Contract_ceilLog2(Contract):
    def specification(self):
        i = self.fresh_var(i64, "i")
        
        self.execute_func(i)

        self.returns_f("lg2 {i}")

class Test_(TestCase):
  def test_(self):
    connect(reset_server=True)
    if __name__ == "__main__": view(LogResults(verbose_failure=True))

    basedir = Path(__file__).absolute().parents[1] # Get absolute path to ceilLog2/
    bcpath = basedir/"artifacts/ceilLog2.bc"
    mod = llvm_load_module(str(bcpath))
    
    ceilLog2_result = llvm_verify(mod, 'ceilLog2', Contract_ceilLog2())
    self.assertTrue(ceilLog2_result.is_success())

if __name__ == "__main__":
    main()
