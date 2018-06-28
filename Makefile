all: build ocamlmerlin ocamlmerlin-server

build:
	jbuilder build

ocamlmerlin ocamlmerlin-server: $(PWD)/_build/install/default/bin/$@
	cp -f $(PWD)/_build/install/default/bin/$@ ./$@

clean:
	jbuilder clean
