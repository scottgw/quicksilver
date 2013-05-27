array_test: array_test.e c.ebc lib/stdlib.bc
	./qsc $< c.ebc

odd_even_test: odd_even_test.e data.ebc incr.ebc lib/stdlib.bc
	./qsc $< data.ebc incr.ebc

separate_test: separate_test.e c.ebc lib/stdlib.bc	
	./qsc $< c.ebc

%.ebc: %.e
	qs $< -o $@

http_test: http_test.e lib/stdlib.bc
	./qsc $<

socket_test: socket_test.e lib/stdlib.bc
	./qsc $<

helloworld: helloworld.e lib/stdlib.bc
	./qsc $<

clean:
	@rm -f helloworld socket_test http_test separate_test *.bc *.ebc *.o *.s

