.PHONY: clean test

main: main.go
	go build -o main

test: main
	./main < main.in > main.out
	diff main.out main.gold
	./main < main.in
	./main < main.in --is_bench

clean:
	go clean
