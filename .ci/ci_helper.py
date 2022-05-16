import cryptol
from cryptol import solver
from prover import *
import random
import sys

def check(c, name, *args):
    result = c.call(name, *args).result()
    if result != True:
        expression = name
        print(name + " failed on '" + expression + "' with arguments");
        for arg in args:
            print(arg)
        sys.exit(1)

def randBV(bits):
    return cryptol.BV(bits, random.getrandbits())

def randList(elements, bits):
    array = []
    element = 0
    while element < elements:
        array.append(randBV(bits))
        element += 1
    return array

def safe(c, name):
    result = c.safe(expr=name, solver=cryptol.solver.W4_YICES).result()
    if result != True:
        print(name + " is not safe")
        sys.exit(1)

def prove(c, name, strProver):
    newProver = selectSolver(strProver)
    result = c.prove(name, solver=newProver, timeout=120.0).result()
    if result != True:
        print(name + " failed to prove: " + result)
        sys.exit(1)
