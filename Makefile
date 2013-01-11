
all: fake-ocamlbuild
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean

FAKE_CMI = location,asttypes,longident,parsetree,clflags
fake-ocamlbuild:
	mkdir -p _build/
	ln -sf $$(ocamlfind query compiler-libs.bytecomp)/{$(FAKE_CMI)}.cmi _build/
