from sys import argv

from cryptol.solver import ABC

from ci_helper import test_cryptol

module_commands = {
    "labs::Demos::Cryptol::Caesar": {
        "check": "v1 v2 v3".split(),
        "prove": [
            (prop, ABC)
            for prop in """
              charIsAtIndex
              recovery_1
              recovery_2
              recovery_3
              recovery_4
              recovery_14
            """.strip().split()
        ],
    },
    "labs::Demos::Cryptol::OneTimePad": {
        "check": ["test"],
    },
    "labs::Demos::Cryptol::OneTimePad": {
        "prove": ["decrypt_of_encrypt_yields_original_plaintext_8_5"],
    },
    "labs::Demos::Cryptol::Sudoku": {
        "prove": """
          check_equiv
          valid_equiv
          puzzle_solution_valid
          puzzle_unique
          hard_solution_valid
          hard_unique
        """.strip().split(),
    },
}

if __name__ == '__main__':
    test_cryptol(module_commands, argv[1:])
