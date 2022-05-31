# Note: Makefile assumes user already started the SAW remote API
# $ start-saw-remote-api

# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

# Note: -g flag compiles the code with debug symbols enabled.
#       The flag lets us pass field names in addition to indices for structs.
all: artifacts
	clang -c -g -emit-llvm -o $(ARTIFACTS)/Game.bc $(SRC)/Game.c
	python3 $(PROOF)/Game.py

artifacts:
	mkdir $(ARTIFACTS)

clean:
	rm -r $(ARTIFACTS)