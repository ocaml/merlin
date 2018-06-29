all: build ocamlmerlin ocamlmerlin-server

dev:
	jbuilder build --dev -j16

build:
	jbuilder build

ocamlmerlin ocamlmerlin-server: $(PWD)/_build/install/default/bin/$@
	cp -f $(PWD)/_build/install/default/bin/$@ ./$@

clean:
	jbuilder clean

.PHONY: all build dev ocamlmerlin ocamlmerlin-server clean
