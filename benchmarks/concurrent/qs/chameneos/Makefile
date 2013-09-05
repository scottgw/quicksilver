.SUFFIXES: .e .ebc

main: main.e chameneos.ebc broker.ebc signal.ebc
	qsc $< chameneos.ebc broker.ebc signal.ebc

.e.ebc:
	qs $< -o $@
