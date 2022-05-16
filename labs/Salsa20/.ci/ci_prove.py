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

c.load_module("labs::Salsa20::Salsa20PropsAnswers")

print("Running proofs in labs::Salsa20::Salsa20PropsAnswers")

prove(c, "rowroundIsInvertibleProp", "z3")
prove(c, "columnroundIsInvertibleProp", "z3")
prove(c, "doubleroundIsInvertibleProp", "z3")
prove(c, "Salsa20CoreEquivProp", "z3")
prove(c, "Salsa20EncryptInvolutionProp_1_1", "z3")
prove(c, "Salsa20EncryptInvolutionProp_1_8", "z3")
prove(c, "Salsa20EncryptInvolutionProp_1_64", "z3")
prove(c, "Salsa20EncryptInvolutionProp_1_128", "z3")
prove(c, "Salsa20EncryptInvolutionProp_2_1", "z3")
prove(c, "Salsa20EncryptInvolutionProp_2_8", "z3")
prove(c, "Salsa20EncryptInvolutionProp_2_64", "z3")
prove(c, "Salsa20EncryptInvolutionProp_2_128", "z3")

