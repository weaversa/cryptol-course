from ci_helper import *
import cryptol

if __name__ == '__main__':
    c = cryptol.connect(reset_server=True)

    properties_by_module = {
        "Esrever": "pi_test",
        "RailFence": "cycle_test pi_test",
        "Scytale": "pi_test",
        "Transposition": "isPermutationMapping_test isPermutation_test",
    }

    for (mod, properties) in properties_by_module.items():
        answers = f"labs::Transposition::{mod}Answers"

        c.load_module(answers)
        print(f"Running tests in {answers}")

        for prop in properties.split():
            check(c, prop)
