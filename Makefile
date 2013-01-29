include Makefile.config

TARGET=ocamlmerlin.native

all: $(TARGET)
	
$(TARGET): fake-ocamlbuild
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean

distclean: clean
	rm -f Makefile.config

install: $(TARGET)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin

FAKE_CMI = location,asttypes,longident,parsetree,clflags,syntaxerr,misc
fake-ocamlbuild:
	mkdir -p _build/
	ln -sf $$(ocamlfind query compiler-libs.bytecomp)/{$(FAKE_CMI)}.cmi _build/
