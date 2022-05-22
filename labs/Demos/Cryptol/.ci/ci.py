from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::Demos::Cryptol::Caesar")

print("Running tests in labs::Demos::Cryptol::Caesar")

check(c, "v1")
check(c, "v2")
check(c, "v3")

c.load_module("labs::Demos::Cryptol::OneTimePad")

print("Running tests in labs::Demos::Cryptol::OneTimePad")

check(c, "test")
