from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::Salsa20::Salsa20Answers")

print("Running proofs in labs::Salsa20::Salsa20Answers")

prove(c, "hexadecimalProp", "z3")
prove(c, "sumProp", "z3")
prove(c, "exclusiveOrProp", "z3")
prove(c, "LeftRotationProp", "z3")
prove(c, "quarterroundIsInjectiveProp", "z3")
prove(c, "rowroundOptProp", "z3")
prove(c, "littleendianInverseProp", "z3")
