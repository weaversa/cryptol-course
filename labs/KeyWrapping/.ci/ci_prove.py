from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::KeyWrapping::KeyWrappingAnswers")

print("Running proofs in labs::KeyWrapping::KeyWrappingAnswers")

prove(c, "WStep2'Prop", "z3")
prove(c, "W'Prop", "z3")
prove(c, "KWAEInvProp", "z3")
