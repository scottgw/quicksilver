.SUFFIXES: .e .ebc

main: main.e condition_worker.ebc data.ebc
	qsc $< condition_worker.ebc data.ebc

.e.ebc:
	qs $< -o $@
