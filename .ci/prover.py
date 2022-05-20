import cryptol
import sys
from cryptol import solver
def selectSolver(prover):
    if prover.upper() == "CVC4":
        return solver.CVC4
    elif prover.upper() == "YICES":
        return solver.YICES
    elif prover.upper() == "Z3":
        return solver.Z3
    elif prover.upper() == "BOOLECTOR":
        return solver.BOOLECTOR
    elif prover.upper() == "MATHSAT":
        return solver.MATHSAT
    elif prover.upper() == "ABC":
        return solver.ABC
    elif prover.upper() == "OFFLINE":
        return solver.OFFLINE
    elif prover.upper() == "ANY":
        return solver.ANY
    elif prover.upper() == "SBV_CVC4":
        return solver.SBV_CVC4
    elif prover.upper() == "SBV_YICES":
        return solver.SBV_YICES
    elif prover.upper() == "SBV_Z3":
        return solver.SBV_Z3
    elif prover.upper() == "SBV_BOOLECTOR":
        return solver.SBV_BOOLECTOR
    elif prover.upper() == "SBV_MATHSAT":
        return solver.SBV_MATHSAT
    elif prover.upper() == "SBV_ABC":
        return solver.SBV_ABC
    elif prover.upper() == "SBV_OFFLINE":
        return solver.SBV_OFFLINE
    elif prover.upper() == "SBV_ANY":
        return solver.SBV_ANY
    elif prover.upper() == "W4_CVC4":
        return solver.W4_CVC4
    elif prover.upper() == "W4_YICES":
        return solver.W4_YICES
    elif prover.upper() == "W4_Z3":
        return solver.W4_Z3
    elif prover.upper() == "W4_BOOLECTOR":
        return solver.W4_BOOLECTOR
    elif prover.upper() == "W4_MATHSAT":
        return solver.W4_MATHSAT
    elif prover.upper() == "W4_ABC":
        return solver.OnlineSolver("w4-abc")
    else:
        print(prover + " is not a valid prover")
        sys.exit(1)
