all: build ocamlmerlin ocamlmerlin-server

build:
	dune build

ocamlmerlin ocamlmerlin-server:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test:
	dune runtest --force

preprocess:
	dune build @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
