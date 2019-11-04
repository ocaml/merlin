all: build ocamlmerlin ocamlmerlin-server ocamlmerlin-lsp

build:
	dune build --always-show-command-line

workspace:
	dune build --always-show-command-line --workspace=dune-workspace.template merlin.install

ocamlmerlin ocamlmerlin-server ocamlmerlin-lsp:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test:
	dune build --always-show-command-line --workspace=dune-workspace.test merlin.install
	dune runtest --workspace=dune-workspace.test --force
	# The following does a "killall ocamlmerlin", it's better not to run it
	# in parallel with the other tests.
	dune build --workspace=dune-workspace.test --force @run-server-test

preprocess:
	dune build --always-show-command-line @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
