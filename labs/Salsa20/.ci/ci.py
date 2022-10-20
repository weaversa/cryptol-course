from sys import argv

from ci_helper import test_cryptol

module_commands = {
    "labs::Salsa20::Salsa20Answers": {
        "prove": """
          hexadecimalProp
          sumProp
          exclusiveOrProp
          LeftRotationProp
          quarterroundIsInjectiveProp
          rowroundOptProp
          littleendianInverseProp
        """.strip().split(),
        "check": """
          quarterroundExamplesProp
          quarterroundIsInjectiveProp
          rowroundExamplesProp
          columnroundExamplesProp
          doubleroundExamplesProp
          littleendianExamplesProp
          Salsa20CoreExamplesProp
          Salsa20ExpansionExamplesProp
          Salsa20EncryptExamplesProp
        """.strip().split(),
        "eval_f": [
            "quarterroundIsInjectiveProp {rand_list(4,32)} {rand_list(4,32)}",
        ],
    },
    "labs::Salsa20::Salsa20PropsAnswers": {
        "prove": """
          rowroundIsInvertibleProp
          columnroundIsInvertibleProp
          doubleroundIsInvertibleProp
          Salsa20CoreEquivProp
          Salsa20EncryptInvolutionProp_1_1
          Salsa20EncryptInvolutionProp_1_8
          Salsa20EncryptInvolutionProp_1_64
          Salsa20EncryptInvolutionProp_1_128
          Salsa20EncryptInvolutionProp_2_1
          Salsa20EncryptInvolutionProp_2_8
          Salsa20EncryptInvolutionProp_2_64
          Salsa20EncryptInvolutionProp_2_128
        """.strip().split(),
        "eval_f": [
            "Salsa20CoreCollidesProp {rand_bv(32)}",
        ],
    },
}

if __name__ == '__main__':
    test_cryptol(module_commands, argv[1:])
