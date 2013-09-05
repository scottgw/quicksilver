.SUFFIXES: .e .ebc

main: main.e producer_worker.ebc consumer_worker.ebc
	qsc $< producer_worker.ebc consumer_worker.ebc

.e.ebc:
	qs $< -o $@
