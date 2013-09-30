.PHONY: test clean

main: main.beam

main.beam: main.erl
	erlc $< -o $@

clean:
	@rm -f main.beam
