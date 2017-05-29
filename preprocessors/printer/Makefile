gen_printer: gen_printer.ml
	ocamlfind opt -o $@ -package unix,menhirSdk -linkpkg $^

clean:
	rm -f gen_printer *.cm*
