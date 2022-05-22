from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::Demos::Cryptol::Caesar")

print("Running tests in labs::Demos::Cryptol::Caesar")

prove(c, "charIsAtIndex", "abc")
prove(c, "recovery_1", "abc")
prove(c, "recovery_2", "abc")
prove(c, "recovery_3", "abc")
prove(c, "recovery_4", "abc")
prove(c, "recovery_14", "abc")

c.load_module("labs::Demos::Cryptol::OneTimePad")

print("Running tests in labs::Demos::Cryptol::OneTimePad")

prove(c, "decrypt_of_encrypt_yields_original_plaintext_8_5", "z3")

c.load_module("labs::Demos::Cryptol::Sudoku")

print("Running tests in labs::Demos::Cryptol::Sudoku")

prove(c, "check_equiv", "z3")
prove(c, "valid_equiv", "z3")
prove(c, "puzzle_solution_valid", "z3")
prove(c, "puzzle_unique", "z3")
prove(c, "hard_solution_valid", "z3")
prove(c, "hard_unique", "z3")
