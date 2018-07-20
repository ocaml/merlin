all: build ocamlmerlin ocamlmerlin-server

dev:
	jbuilder build --dev -j16

build:
	jbuilder build

ocamlmerlin ocamlmerlin-server:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	jbuilder clean

test:
	jbuilder runtest

promote:
	jbuilder promote

.PHONY: all build dev clean test promote
