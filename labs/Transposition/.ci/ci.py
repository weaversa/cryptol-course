from ci_helper import *
import cryptol

c = cryptol.connect(reset_server=True)

c.load_module("labs::Transposition::EsreverAnswers")

print("Running tests in labs::Transposition::EsreverAnswers")

check(c, "pi_test")

c.load_module("labs::Transposition::RailFenceAnswers")

print("Running tests in labs::Transposition::RailFenceAnswers")

check(c, "cycle_test")
check(c, "pi_test")

c.load_module("labs::Transposition::ScytaleAnswers")

print("Running tests in labs::Transposition::ScytaleAnswers")

check(c, "pi_test")

c.load_module("labs::Transposition::TranspositionAnswers")

print("Running tests in labs::Transposition::ScytaleAnswers")

check(c, "isPermutation_test")
check(c, "isPermutationMapping_test")
