# Note: Makefile assumes user already started the SAW remote API
# $ start-saw-remote-api

# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

all: build

prove: build
	python3 $(PROOF)/null.py

build:
	mkdir -p $(ARTIFACTS)
	clang -c -g -emit-llvm -o $(ARTIFACTS)/null.bc $(SRC)/null.c

clean:
	rm -r $(ARTIFACTS)
