all:
	ocamlbuild -use-ocamlfind test.byte

clean:
	ocamlbuild -clean
