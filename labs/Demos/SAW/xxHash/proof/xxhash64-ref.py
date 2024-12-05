from pathlib import Path
import unittest
from saw_client import *
from saw_client.crucible import cry_f
from saw_client.llvm import Contract, SetupVal, FreshVar, i8, i32, i64, null
from saw_client.proofscript import ProofScript, yices
from saw_client.llvm_type import LLVMType, LLVMArrayType

def ptr_to_fresh(c : Contract, ty : LLVMType, name : Optional[str] = None, read_only : Optional[bool] = False) -> Tuple[FreshVar, SetupVal]:
    """Add to``Contract`` ``c`` an allocation of a pointer of type ``ty`` initialized to an unknown fresh value.
    :returns A fresh variable bound to the pointers initial value and the newly allocated pointer. (The fresh
             variable will be assigned ``name`` if provided/available.)"""
    var = c.fresh_var(ty, name)
    ptr = c.alloc(ty, points_to = var, read_only = read_only)
    return (var, ptr)

class Contract_XXH_rotl64(Contract):
    def specification(self) -> None:
        value = self.fresh_var(i64, "value")
        amt = self.fresh_var(i32, "amt")

        self.precondition_f("{amt} > 0")
        self.precondition_f("{amt} < 64")
        
        self.execute_func(value, amt)

        self.returns_f("{value} <<< {amt}")


class Contract_XXH64_round(Contract):
    def specification(self) -> None:
        acc = self.fresh_var(i64, "acc")
        input_ = self.fresh_var(i64, "input")

        self.execute_func(acc, input_)

        self.returns_f("XXH64_round {acc} {input_}")

      
class Contract_XXH64_avalanche(Contract):
    def specification(self) -> None:
        hash_ = self.fresh_var(i64, "hash")

        self.execute_func(hash_)

        self.returns_f("XXH64_avalanche {hash_}")


class Contract_XXH64_NULL (Contract):
    def specification(self) -> None:
        length = self.fresh_var(i64, "length")
        seed = self.fresh_var(i64, "seed")

        self.execute_func(null(), length, seed)

        self.returns_f("XXH64`{{L=0}} zero {seed}")


class Contract_XXH64_top(Contract):
    def __init__(self, size):
        super().__init__()
        self.size = size
        
    def specification(self) -> None:
        (input, input_p) = ptr_to_fresh(self, LLVMArrayType(i8, self.size), read_only = True) 
        length = cry_f("{self.size} : [64]")
        seed = self.fresh_var(i64, "seed")

        self.execute_func(input_p, length, seed)

        self.returns_f("XXH64`{{L={self.size}}} {input} {seed}")

    
class xxhash64EasyTest(unittest.TestCase):
    def test_xxhash64(self):
        connect(reset_server=True)
        if __name__ == "__main__": view(LogResults())

        basedir = Path(__file__).absolute().parents[1] # Get absolute path to Salsa20/ 
        cryname = basedir/"specs/xxhash.cry"
        bcname  = basedir/"artifacts/xxhash64-ref.bc"

        cryptol_load_file(str(cryname))

        mod = llvm_load_module(str(bcname))

        XXH_rotl64_result = llvm_verify(mod, "XXH_rotl64", Contract_XXH_rotl64(), script=ProofScript([yices([])]), check_sat=True)
        self.assertIs(XXH_rotl64_result.is_success(), True)

        XXH64_round_result = llvm_verify(mod, "XXH64_round", Contract_XXH64_round(), script=ProofScript([yices([])]), check_sat=True)
        self.assertIs(XXH64_round_result.is_success(), True)        

        XXH64_avalanche_result = llvm_verify(mod, "XXH64_avalanche", Contract_XXH64_avalanche(), script=ProofScript([yices([])]), check_sat=True)
        self.assertIs(XXH64_avalanche_result.is_success(), True)

        XXH64_NULL_result = llvm_verify(mod, "XXH64", Contract_XXH64_NULL(), script=ProofScript([yices([])]), check_sat=True)
        self.assertIs(XXH64_NULL_result.is_success(), True)

        def XXH64_verify_print(size):
            XXH64_result = llvm_verify(mod, "XXH64", Contract_XXH64_top(size), lemmas=[XXH_rotl64_result, XXH64_round_result, XXH64_avalanche_result], script=ProofScript([yices([])]), check_sat=True)
            self.assertIs(XXH64_result.is_success(), True)
        
        for i in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 32, 64, 128]:
            XXH64_verify_print(i)

if __name__ == "__main__":
    unittest.main()
