.SUFFIXES: .e .ebc

main: main.e mutex_worker.ebc data.ebc
	qsc $< mutex_worker.ebc data.ebc

.e.ebc:
	qs $< -o $@
