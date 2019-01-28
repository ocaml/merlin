all: build ocamlmerlin ocamlmerlin-server ocamlmerlin-lsp

build:
	dune build

ocamlmerlin ocamlmerlin-server ocamlmerlin-lsp:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test: build
	dune runtest --force

preprocess:
	dune build @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
