EFILES=pointer_8 prelude pointer string
EBCFILES=$(EFILES:=.ebc)

stdlib.bc: externals.cbc ${EBCFILES}
	llvm-link $^ -o $@

%.ebc: %.e
	qs $< -o $@

%.cbc: %.c
	clang -emit-llvm -c $< -o $@
