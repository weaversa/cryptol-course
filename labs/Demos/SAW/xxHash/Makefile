# Note: Makefile assumes user already started the SAW remote API
# $ start-saw-remote-api

# Variables to hold relative file paths
SRC=src
PROOF=proof
SPECS=specs
ARTIFACTS=artifacts

# Lab name
LAB1=xxhash32-ref
LAB2=xxhash64-ref

all: build

build:
	$(ARTIFACTS)/$(LAB1).bc
	$(ARTIFACTS)/$(LAB2).bc

prove: build
	python3 $(PROOF)/$(LAB1).py
	python3 $(PROOF)/$(LAB2).py

clean:
	rm -rf $(ARTIFACTS)

.PHONY: all build prove clean

$(ARTIFACTS)/$(LAB1).bc: $(SRC)/$(LAB1).c | $(ARTIFACTS)
	clang $< -o $@ -c -emit-llvm -O0 -std=c90

$(ARTIFACTS)/$(LAB2).bc: $(SRC)/$(LAB2).c | $(ARTIFACTS)
	clang $< -o $@ -c -emit-llvm -O0 -std=c90

$(ARTIFACTS):
	mkdir -p $(ARTIFACTS)
