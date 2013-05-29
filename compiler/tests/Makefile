all: condition_test mutex_test prodcons_test share_test\
	array_test odd_even_test separate_test http_test socket_test helloworld

share_test: share_test.e data.ebc share_worker.ebc lib/stdlib.bc
	./qsc $< data.ebc share_worker.ebc

prodcons_test: prodcons_test.e producer_worker.ebc consumer_worker.ebc lib/stdlib.bc
	./qsc $< producer_worker.ebc consumer_worker.ebc

mutex_test: mutex_test.e data.ebc mutex_worker.ebc lib/stdlib.bc
	./qsc $< data.ebc mutex_worker.ebc

condition_test: condition_test.e data.ebc condition_worker.ebc lib/stdlib.bc
	./qsc $< data.ebc condition_worker.ebc

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
	@rm -f helloworld socket_test http_test separate_test array_test odd_even_test\
		condition_test mutex_test prodcons_test share_test \
		*.bc *.ebc *.o *.obc *.s

