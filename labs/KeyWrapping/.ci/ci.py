from sys import argv

from ci_helper import test_cryptol

module_commands = {
    "labs::KeyWrapping::KeyWrappingAnswers": {
        "prove": """
          WStep2'Prop
          W'Prop
          KWAEInvProp
        """.strip().split(),
        "check": """
          hexadecimalProp
          zeroBitsProp
          concatenationProp
          XORProp
          lenProp
          LSBProp
          MSBProp
          bitstringProp
          intProp

          KWAETests
          KWADTests
          TKWAETests
          TKWADTests
          KWPAETests
          KWPADTests
        """.strip().split(),
    },
}

if __name__ == '__main__':
    test_cryptol(module_commands, argv[1:])
