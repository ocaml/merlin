all: build ocamlmerlin ocamlmerlin-server

dev:
	jbuilder build --dev -j16

build:
	jbuilder build

_build/install/default/bin/ocamlmerlin _build/install/default/bin/ocamlmerlin-server: build

ocamlmerlin ocamlmerlin-server: _build/install/default/bin/$@
	cp -f _build/install/default/bin/$@ ./$@

clean:
	jbuilder clean

.PHONY: all build dev ocamlmerlin ocamlmerlin-server clean
