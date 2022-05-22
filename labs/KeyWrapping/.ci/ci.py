from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::KeyWrapping::KeyWrappingAnswers")

print("Running tests in labs::KeyWrapping::KeyWrappingAnswers")

check(c, "hexadecimalProp")
check(c, "zeroBitsProp")
check(c, "concatenationProp")
check(c, "XORProp")
check(c, "lenProp")
check(c, "LSBProp")
check(c, "MSBProp")
check(c, "bitstringProp")
check(c, "intProp")

check(c, "KWAETests")
check(c, "KWADTests")
check(c, "TKWAETests")
check(c, "TKWADTests")
check(c, "KWPAETests")
check(c, "KWPADTests")
