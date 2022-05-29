# Note: Makefile assumes user already started the SAW remote API
# $ start-saw-remote-api

# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

all: artifacts
	clang -c -emit-llvm -o $(ARTIFACTS)/Game.bc $(SRC)/Game.c
	python3 $(PROOF)/Game.py

artifacts:
	mkdir $(ARTIFACTS)

clean:
	rm -r $(ARTIFACTS)