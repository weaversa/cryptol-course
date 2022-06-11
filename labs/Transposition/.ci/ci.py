from sys import argv

from ci_helper import test_cryptol

module_commands = {
    f"labs::Transposition::{mod}Answers": {
        "check": props.split()
    }
    for (mod, props) in {
        "Esrever": "pi_test",
        "RailFence": "cycle_test pi_test",
        "Scytale": "pi_test",
        "Transposition": "isPermutationMapping_test isPermutation_test",
    }.items()
}

if __name__ == '__main__':
    test_cryptol(module_commands, argv[1:])
