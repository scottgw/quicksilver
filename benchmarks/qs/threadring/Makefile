.SUFFIXES: .e .ebc

main: main.e threadring_worker.ebc token.ebc
	qsc $< threadring_worker.ebc token.ebc

.e.ebc:
	qs $< -o $@
