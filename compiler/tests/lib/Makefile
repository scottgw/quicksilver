EFILES=pointer_8 pointer_64 prelude pointer string socket_util \
       real_math \
	int_array int_matrix \
	real_array real_matrix \
	int_stack int_stack_link \
	socket socket_buffer file \
	array array_inner
EBCFILES=$(EFILES:=.ebc)

stdlib.bc: externals.cbc ${EBCFILES}
	llvm-link $^ -o $@

%.ebc: %.e
	qs $< -o $@

%.cbc: %.c
	clang -pthread -emit-llvm -c $< -o $@ -Wall  `pkg-config --cflags libqs`

clean:
	@rm -f *.bc *.ebc *.cbc
