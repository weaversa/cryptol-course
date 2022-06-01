import unittest
from saw_client             import *
from saw_client.crucible    import *
from saw_client.llvm        import *
from saw_client.proofscript import *
from saw_client.llvm_type   import *

def ptr_to_fresh(c : Contract, ty : LLVMType, name : Optional[str] = None, read_only : Optional[bool] = False) -> Tuple[FreshVar, SetupVal]:
    var = c.fresh_var(ty, name)
    ptr = c.alloc(ty, points_to = var, read_only=read_only)
    return (var, ptr)

class addRow5Mutate_Contract(Contract):
    def specification(self):
        (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
        (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
        
        self.execute_func(a_p, b_p)
        
        self.points_to(a_p, cry_f("addRow {a} {b}"))
        self.returns(void)

class addRow5NewVar_Contract(Contract):
    def specification(self):
        (a, a_p) = ptr_to_fresh(self, array_ty(5, i32), name="a")
        (b, b_p) = ptr_to_fresh(self, array_ty(5, i32), name="b", read_only=True)
        
        self.execute_func(a_p, b_p)
        
        (c, c_p) = ptr_to_fresh(self, array_ty(5, i32), name="c")
        self.points_to(c_p, cry_f("addRow {a} {b}"))
        
        self.returns(c_p)
        
class addRowAlias_Contract(Contract):
    def __init__(self, length : int):
        super().__init__()
        self.length = length

    def specification(self):
        (a, a_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="a")
        (b, b_p) = ptr_to_fresh(self, array_ty(self.length, i32), name="b", read_only=True)
        length   = self.fresh_var(i8, "length")

        self.precondition_f("{length} == {self.length}")

        self.execute_func(a_p, b_p, length)

        self.points_to(a_p, cry_f("addRow`{{{self.length}}} {a} {b}"))

        self.returns(a_p)

class ArrayTests(unittest.TestCase):
    def test_rowAdds(self):
        connect(reset_server=True)
        if __name__ == "__main__": view(LogResults(verbose_failure=True))

        bcname  = "../src/addRow.bc"
        cryname = "spec/addRow.cry"

        cryptol_load_file(cryname)
        mod = llvm_load_module(bcname)

        addRow5Mutate_result = llvm_verify(mod, 'addRow5Mutate', addRow5Mutate_Contract())
        self.assertIs(addRow5Mutate_result.is_success(), True)
        
        addRow5NewVar_result = llvm_verify(mod, 'addRow5NewVar', addRow5NewVar_Contract())
        self.assertIs(addRow5NewVar_result.is_success(), True)
        
        addRowAlias05_result = llvm_verify(mod, 'addRowAlias', addRowAlias_Contract(5))
        self.assertIs(addRowAlias05_result.is_success(), True)

if __name__ == "__main__":
    unittest.main()
