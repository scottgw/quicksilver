.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

main.beam: main.erl

clean:
	@rm -f main.beam
