all: array_test odd_even_test separate_test http_test socket_test helloworld \
	passive1 multi_reserve

multi_reserve: multi_reserve.e data.ebc lib/stdlib.bc
	qsc $< data.ebc

array_test: array_test.e c.ebc lib/stdlib.bc
	qsc $< c.ebc

odd_even_test: odd_even_test.e data.ebc incr.ebc lib/stdlib.bc
	qsc $< data.ebc incr.ebc

separate_test: separate_test.e c.ebc lib/stdlib.bc	
	qsc $< c.ebc

passive1: passive1.e c.ebc lib/stdlib.bc
	qsc $< c.ebc

%.ebc: %.e
	qs $< -o $@

http_test: http_test.e lib/stdlib.bc
	qsc $<

socket_test: socket_test.e lib/stdlib.bc
	qsc $<

helloworld: helloworld.e lib/stdlib.bc
	qsc $<

clean:
	@rm -f helloworld socket_test http_test separate_test array_test odd_even_test\
		condition_test mutex_test prodcons_test share_test \
		*.bc *.ebc *.o *.obc *.s

