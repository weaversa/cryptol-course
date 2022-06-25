# Note: Makefile assumes user already started the SAW remote API
# $ start-saw-remote-api

# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

all: build

build: $(ARTIFACTS)/null.bc

prove: build
	python3 $(PROOF)/null.py

clean:
	rm -r $(ARTIFACTS)

.PHONY: all build prove clean

$(ARTIFACTS)/null.bc: $(SRC)/null.c | $(ARTIFACTS)
	clang -c -g -emit-llvm -o $@ $<

$(ARTIFACTS):
	mkdir -p $(ARTIFACTS)