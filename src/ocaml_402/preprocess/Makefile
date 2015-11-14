MENHIR_FLAGS = --infer --inspection --table --cmly \
	--ocamlc 'ocamlc.opt -I ../parsing -I ../../ocaml_aux -I ../../kernel'

.PHONY: raw_recover.ml

all: raw_recover.ml

raw_parser.mli raw_parser.cmly raw_parser.ml: raw_parser.mly
	menhir $(MENHIR_FLAGS) raw_parser.mly

raw_recover.ml: gen_recover raw_parser.cmly
	./gen_recover raw_parser.cmly > raw_recover.ml

raw_printer.ml: gen_printer raw_parser.cmly
	./gen_printer raw_parser.cmly > raw_printer.ml

gen_recover: gen_recover.ml
	ocamlfind opt -o $@ -package menhirSdk -linkpkg $^

gen_printer: gen_printer.ml
	ocamlfind opt -o $@ -package menhirSdk -linkpkg $^

clean:
	rm -f *.cm* *.o gen_recover gen_printer
