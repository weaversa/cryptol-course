from sys import argv

from ci_helper import test_cryptol

module_commands = {
    "labs::SimonSpeck::SpeckAnswers::SpeckTestVectors": {
        "check": ["all_speck_vectors_pass"],
    },
    "labs::SimonSpeck::Simon::SimonTestVectors": {
        "check": ["all_simon_vectors_pass"],
    },
}

if __name__ == '__main__':
    test_cryptol(module_commands, argv[1:])
