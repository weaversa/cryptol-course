# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

all: artifacts
	clang -c -emit-llvm -o $(ARTIFACTS)/Player.bc $(SRC)/Player.c
	python3 $(PROOF)/Player.py

artifacts:
	mkdir $(ARTIFACTS)

clean:
	rm -r $(ARTIFACTS)