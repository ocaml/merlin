OCAMLMAKEFILE = ../../OCamlMakefile

all: native-code

OCamlMakefile:
	ln -sf ../../$@ $@

SOURCES = \
  utils.ml                             \
  recover_attrib.mli recover_attrib.ml \
  synthesis.mli synthesis.ml           \
  emitter.mli emitter.ml               \
  recovery.mli recovery.ml             \
  main.ml

RESULT = gen_recover
PACKS = menhirSdk

-include $(OCAMLMAKEFILE)
