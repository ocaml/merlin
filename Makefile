all: build ocamlmerlin ocamlmerlin-server dot-merlin-reader

build:
	dune build --always-show-command-line

ocamlmerlin ocamlmerlin-server dot-merlin-reader:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test: build
	dune runtest

preprocess:
	dune build --always-show-command-line @preprocess

promote:
	dune promote

bench:
	merl-an benchmark -p /projects/irmin -s 1 --data=merl-an_bench
	jq . merl-an_bench/bench.json

.PHONY: all build dev clean test promote bench
