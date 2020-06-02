BINARY_NAME=arithmetic_unit_tests
CC=clang


all:
	$(CC) -o $(BINARY_NAME) arithmetic_implementations.c
	$(CC) -g -c -emit-llvm -o arithmetic_llvm.bc arithmetic_implementations.c

add_tests:
	saw add_llvm.saw

multiply_tests:
	saw multiply_llvm.saw

clean:
	rm -f *.bc $(BINARY_NAME) *~
