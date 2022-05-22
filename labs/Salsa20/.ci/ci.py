from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::Salsa20::Salsa20Answers")

print("Running tests in labs::Salsa20::Salsa20Answers")

check(c, "quarterroundExamplesProp")
check(c, "quarterroundIsInjectiveProp", randList(4, 32), randList(4, 32))
check(c, "rowroundExamplesProp")
check(c, "columnroundExamplesProp")
check(c, "doubleroundExamplesProp")
check(c, "littleendianExamplesProp")
check(c, "Salsa20CoreExamplesProp")
check(c, "Salsa20ExpansionExamplesProp")
check(c, "Salsa20EncryptExamplesProp")


c.load_module("labs::Salsa20::Salsa20PropsAnswers")

print("Running proofs in labs::Salsa20::Salsa20PropsAnswers")

check(c, "Salsa20CoreCollidesProp", randBV(32))
