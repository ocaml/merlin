MENHIR_FLAGS = --infer --inspection --table --cmly \
	--ocamlc 'ocamlc.opt -I ../parsing -I ../../ocaml_aux -I ../../kernel'

.PHONY: parser_recover.ml

all: parser_recover.ml parser_printer.ml

parser_raw.mli parser_raw.cmly parser_raw.ml: parser_raw.mly
	menhir $(MENHIR_FLAGS) parser_raw.mly

parser_recover.ml: gen_recover parser_raw.cmly
	./gen_recover parser_raw.cmly > ../parser_recover.ml

parser_printer.ml: gen_printer parser_raw.cmly
	./gen_printer parser_raw.cmly > ../parser_printer.ml

gen_recover: gen_recover.ml
	ocamlfind opt -g -o $@ -package menhirSdk -linkpkg $^

gen_printer: gen_printer.ml
	ocamlfind opt -g -o $@ -package menhirSdk -linkpkg $^

clean:
	rm -f *.cm* *.o gen_recover gen_printer

makeBitSet.cmi: makeBitSet.mli
	ocamlfind opt -g -package menhirSdk -c makeBitSet.mli

makeBitSet.cmx: makeBitSet.ml makeBitSet.cmi
	ocamlfind opt -g -package menhirSdk -c makeBitSet.ml

gen_recover2: makeBitSet.cmx gen_recover2.ml
	ocamlfind opt -g -package menhirSdk -linkpkg makeBitSet.cmx \
		gen_recover2.ml -o gen_recover2

recover: gen_recover2
	./gen_recover2 -v parser_raw.cmly
