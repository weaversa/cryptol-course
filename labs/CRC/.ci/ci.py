from sys import argv

from ci_helper import test_cryptol

module_commands = {
    "labs::CRC::CRCAnswers": {
        "check": """
          CRCSimpleTest
          CRCSimple_QTest
          CRCSimple_XFERTest
          CRC32Test
          CRC32_BZIP2Test
          CRC32_CTest
          CRC32_DTest
          CRC32_MPEG2Test
          CRC32_POSIXTest
          CRC32_QTest
          CRC32_JAMCRCTest
          CRC32_XFERTest
        """.strip().split(),
    },
}

if __name__ == '__main__':
    test_cryptol(module_commands, argv[1:])
