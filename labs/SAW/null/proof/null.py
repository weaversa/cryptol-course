import os
import unittest
from saw_client          import *
from saw_client.crucible import *
from saw_client.llvm     import *

class isNull_Contract(Contract):
    def specification(self):
        # Lab: Explain why the following 2 lines make the contract fail
        # p = self.alloc(i32)
        # self.execute_func(p)

        self.execute_func(null())

        self.returns(cry("1 : [32]"))

class LLVMAssertNullTest(unittest.TestCase):
    def test_llvm_assert_null(self):
        connect(reset_server=True)
        if __name__ == "__main__": view(LogResults(verbose_failure=True))

        pwd = os.getcwd()
        bcname = pwd + "/artifacts/null.bc"
        mod    = llvm_load_module(bcname)

        result = llvm_verify(mod, 'isNull', isNull_Contract())
        self.assertIs(result.is_success(), True)

if __name__ == "__main__":
    unittest.main()
