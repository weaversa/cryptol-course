# Variables to hold relative file paths
SRC=src
PROOF=proof
ARTIFACTS=artifacts

all:
	mkdir $(ARTIFACTS)
	clang -c -emit-llvm -o $(ARTIFACTS)/Player.bc $(SRC)/Player.c

clean:
	rm -r $(ARTIFACTS)