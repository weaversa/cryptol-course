from ci_helper import *
import cryptol

c = cryptol.connect()

c.load_module("labs::CRC::CRCAnswers")

print("Running tests in labs::CRC::CRCAnswers")

check(c, "CRCSimpleTest")
check(c, "CRCSimple_QTest")
check(c, "CRCSimple_XFERTest")
check(c, "CRC32Test")
check(c, "CRC32_BZIP2Test")
check(c, "CRC32_CTest")
check(c, "CRC32_DTest")
check(c, "CRC32_MPEG2Test")
check(c, "CRC32_POSIXTest")
check(c, "CRC32_QTest")
check(c, "CRC32_JAMCRCTest")
check(c, "CRC32_XFERTest")


