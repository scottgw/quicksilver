.PHONY: test clean

main: main.beam

main.beam: main.erl
	erlc -smp  $< -o $@

clean:
	@rm -f main.beam
