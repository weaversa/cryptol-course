from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::SimonSpeck::SpeckAnswers::SpeckTestVectors")

print("Running tests in labs::SimonSpeck::SpeckAnswers::SpeckTestVectors")

check(c, "all_speck_vectors_pass")

c.load_module("labs::SimonSpeck::Simon::SimonTestVectors")

print("Running tests in labs::SimonSpeck::Simon::SimonTestVectors")

check(c, "all_simon_vectors_pass")
