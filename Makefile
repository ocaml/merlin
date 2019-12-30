all: build ocamlmerlin ocamlmerlin-server dot-merlin-reader

build:
	dune build --always-show-command-line

workspace:
	dune build --always-show-command-line --workspace=dune-workspace.template

ocamlmerlin ocamlmerlin-server dot-merlin-reader:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test:
	dune build --always-show-command-line --workspace=dune-workspace.test
	dune runtest --workspace=dune-workspace.test

test-current:
	dune build --always-show-command-line
	dune runtest

preprocess:
	dune build --always-show-command-line @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
