MENHIR_FLAGS = --infer --inspection --table --cmly \
	--ocamlc 'ocamlc.opt -I ../parsing -I ../../ocaml_aux -I ../../kernel'

.PHONY: parser_recover.ml parser_printer.ml

all: parser_printer.ml #parser_recover.ml 

parser_raw.mli parser_raw.cmly parser_raw.ml: parser_raw.mly
	menhir $(MENHIR_FLAGS) parser_raw.mly

#parser_recover.ml: gen_recover parser_raw.cmly
#	./gen_recover parser_raw.cmly > ../parser_recover.ml

parser_printer.ml: gen_printer parser_raw.cmly
	./gen_printer parser_raw.cmly > ../parser_printer.ml

gen_printer: gen_printer.ml
	ocamlfind opt -g -o $@ -package menhirSdk -linkpkg $^

clean:
	rm -f *.cm* *.o gen_printer
