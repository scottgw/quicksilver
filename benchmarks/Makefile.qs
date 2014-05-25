.SUFFIXES: .e .ebc

export QS_LIB_PATH = ../../../../compiler/tests/lib

.PHONY: all

all: main main_justqoq main_juststat main_none

# all files, including main.e
ALL_SRCS=$(wildcard *.e)

# without main.e
SRCS=$(filter-out main.e,$(ALL_SRCS))

# requried bytecode files
BC_OBJS=$(SRCS:.e=.ebc)

main: main.e $(BC_OBJS)
	qsc -o $@ $< $(BC_OBJS)

main_justqoq: main.e $(BC_OBJS)
	qsc -l -o $@ $< $(BC_OBJS)

main_juststat: main.e $(BC_OBJS)
	qsc -q -o $@ $< $(BC_OBJS)

main_none: main.e $(BC_OBJS)
	qsc -l -q -o $@ $< $(BC_OBJS)

.e.ebc:
	qs $< -o $@

clean:
	@rm -f *.ebc *.bc main main_nolift main_noqoq main_noliftqoq
