# README tests
cryptol -e -c ":m README" -c ":prove"

# Interpreter lab tests
cryptol -e -c ":m labs::Interpreter::Interpreter" -c ":s base=10" -c "x + 2"

# Arithmetic verification tests
# "! command -v saw >/dev/null 2>&1 || make -C labs/Demos/SAW/ArithmeticVerifications"

# Bittwiddling lab tests
cryptol -e -c ":m labs::Demos::SAW::Bittwiddling::Bittwiddling"
# clang -emit-llvm -c labs/Demos/SAW/Bittwiddling/bittwiddling.c -o labs/Demos/SAW/Bittwiddling/bittwiddling.bc
cryptol -e -c ":m labs::Demos::SAW::Bittwiddling::BittwiddlingAnswers"
# "! command -v saw >/dev/null 2>&1 || (cd labs/Demos/SAW/Bittwiddling && saw BittwiddlingAnswers.saw)"

# Salsa20 lab tests
cryptol -e -c ":m labs::Salsa20::Salsa20"
cryptol -e -c ":m labs::Salsa20::Salsa20Answers"
cryptol -e -c ":m labs::Salsa20::Salsa20Props"
cryptol -e -c ":m labs::Salsa20::Salsa20PropsAnswers"
cryptol -e -c ":m labs::Salsa20::spec::Salsa20"

# CRC lab tests
cryptol -e -c ":m labs::CRC::CRC"
cryptol -e -c ":m labs::CRC::CRCAnswers"
cryptol -e -c ":m labs::CRC::spec::CRC"

# LoremIpsum tests
cryptol -e -c ":m labs::LoremIpsum::LoremIpsum"
cryptol -e -c ":m labs::LoremIpsum::LoremIpsumAnswers"
cryptol -e -c ":m labs::LoremIpsum::KLI20"

# Cryptol demo lab tests
cryptol -e -c ":m labs::Demos::Cryptol::Caesar"
cryptol -e -c ":m labs::Demos::Cryptol::OneTimePad"
cryptol -e -c ":m labs::Demos::Cryptol::NQueens"
cryptol -e -c ":m labs::Demos::Cryptol::Sudoku"

# Overview lab tests
cryptol -e -c ":m labs::Overview::Overview"

# Language Basics lab tests
cryptol -e -c ":m labs::Language::Basics"
cryptol -e -c ":m labs::Language::IntroTypeHackery"

# Project Euler lab tests
cryptol -e -c ":m labs::ProjectEuler::ProjectEuler"
cryptol -e -c ":m labs::ProjectEuler::ProjectEulerAnswers"
cryptol -e -c ":m labs::ProjectEuler::keylog"
cryptol -e -c ":m labs::ProjectEuler::cipher1"
cryptol -e -c ":m labs::ProjectEuler::cipher2"

# Crypto Proofs lab tests
cryptol -e -c ":m labs::CryptoProofs::CryptoProofs"
cryptol -e -c ":m labs::CryptoProofs::CryptoProofsAnswers"

# Key Wrapping lab tests
cryptol -e -c ":m labs::KeyWrapping::KeyWrapping"
cryptol -e -c ":m labs::KeyWrapping::KeyWrappingAnswers"

# Simon and Speck lab tests
cryptol -e -c ":m labs::SimonSpeck::SimonSpeck"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_64_96"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_128_256"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_32_64"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_48_96"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_128_192"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_96_96"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_48_72"
cryptol -e -c ":m labs::SimonSpeck::Simon::Simon"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_128_128"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_64_128"
cryptol -e -c ":m labs::SimonSpeck::Simon::simon_96_144"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_128_192"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_128_256"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_96_96"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_48_72"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_128_128"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_32_64"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_48_96"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::Speck"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_64_96"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_96_144"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::speck_64_128"
cryptol -e -c ":m labs::SimonSpeck::SpeckAnswers::SpeckTestVectors"

# Transpostion Lab Tests
cryptol -e -c ":m labs::Transposition::CommonProperties"
cryptol -e -c ":m labs::Transposition::CommonPropertiesAnswers"
cryptol -e -c ":m labs::Transposition::Transposition"
cryptol -e -c ":m labs::Transposition::TranspositionAnswers"
cryptol -e -c ":m labs::Transposition::Esrever"
cryptol -e -c ":m labs::Transposition::EsreverAnswers"
cryptol -e -c ":m labs::Transposition::Scytale"
cryptol -e -c ":m labs::Transposition::ScytaleAnswers"
cryptol -e -c ":m labs::Transposition::RailFence"
cryptol -e -c ":m labs::Transposition::RailFenceAnswers"
cryptol -e -c ":m labs::Transposition::Route"
