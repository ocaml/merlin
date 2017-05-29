gen_explain: gen_explain.ml
	ocamlfind opt -o $@ -package unix,menhirSdk -linkpkg $^

clean:
	rm -f gen_explain *.cm*
