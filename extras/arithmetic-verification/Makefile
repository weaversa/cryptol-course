BINARY_NAME=arithmetic_unit_tests
CC=clang


all: add_tests

$(BINARY_NAME): arithmetic_implementations.c
	$(CC) -o $(BINARY_NAME) arithmetic_implementations.c

arithmetic_llvm.bc: arithmetic_implementations.c
	$(CC) -g -c -emit-llvm -o arithmetic_llvm.bc arithmetic_implementations.c

add_tests: arithmetic_llvm.bc
	saw add_llvm.saw

multiply_tests: arithmetic_llvm.bc
	saw multiply_llvm.saw

clean:
	rm -f *.bc $(BINARY_NAME) *~
