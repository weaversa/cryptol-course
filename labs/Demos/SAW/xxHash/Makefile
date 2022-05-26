all: prove

prove: xxhash32-ref.bc xxhash64-ref.bc
	saw xxhash32-ref.saw
	saw xxhash64-ref.saw

%ref.bc: %ref.c
	clang $< -o $@ -c -emit-llvm -O0 -std=c90

clean:
	rm -rf xxhash32-ref.bc xxhash64-ref.bc

.PHONY: all clean prove
