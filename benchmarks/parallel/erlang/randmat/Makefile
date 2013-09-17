.PHONY: test

main: main.beam

test: main.beam
	erl -noshell -s main main -s init stop < main.in
	echo "1 4 8" | erl -noshell -s main main -s init stop
	echo "1 5 8" | erl -noshell -s main main -s init stop
	echo "2 5 8" | erl -noshell -s main main -s init stop
	echo "2 5 9" | erl -noshell -s main main -s init stop
	echo "2 5 9" | erl -noshell -s main main is_bench -s init stop

main.beam: main.erl
	erlc +native +"{hipe,[o3]}"  $< -o $@

clean:
	@rm -f main.beam
