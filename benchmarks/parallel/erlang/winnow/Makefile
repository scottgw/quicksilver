.PHONY: test clean

main: main.beam

main.beam: main.erl
	erlc +native +"{hipe,[o3]}"  $< -o $@

clean:
	@rm -f main.beam
