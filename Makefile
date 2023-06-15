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
	opam pin -y merl-an https://github.com/pitag-ha/merl-an.git
	git clone https://github.com/pitag-ha/merl-an.git
	dune build @install
	merl-an benchmark -s 2 -p merl-an --data=merl-an_bench
	cat merl-an_bench/bench.json

.PHONY: all build dev clean test promote bench
