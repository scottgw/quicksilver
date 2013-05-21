separate_test: separate_test.e c.ebc lib/stdlib.bc	
	./qsc $< c.ebc

c.ebc: c.e
	qs c.e -o c.ebc

http_test: http_test.e lib/stdlib.bc
	./qsc $<

socket_test: socket_test.e lib/stdlib.bc
	./qsc $<

helloworld: helloworld.e lib/stdlib.bc
	./qsc $<

clean:
	@rm -f helloworld socket_test http_test separate_test *.bc *.ebc *.o *.s

