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

.PHONY: all build dev clean test
