# Note: Makefile assumes user already started the SAW remote API
# $ start-saw-remote-api

# Lab name
LAB=addRow

# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

all: build

build: $(ARTIFACTS)/$(LAB).bc

prove: build
	python3 $(PROOF)/$(LAB).py

clean:
	rm -rf $(ARTIFACTS)

.PHONY: all build prove clean

$(ARTIFACTS)/$(LAB).bc: $(SRC)/$(LAB).c | $(ARTIFACTS)
	clang -c -g -emit-llvm -o $@ $<

$(ARTIFACTS):
	mkdir -p $(ARTIFACTS)