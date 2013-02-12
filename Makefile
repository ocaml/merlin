include Makefile.config

TARGET = ocamlmerlin.native

DISTNAME = ocamlmerlin-0.1
DISTFILES = configure Makefile README _tags vim $(wildcard *.ml *.mli *.mly *.mll)

OCAMLBUILD=ocamlbuild -Is .,typing,parsing,utils
OCAMLFIND=ocamlfind

all: $(TARGET)
	
$(TARGET): fake-ocamlbuild
	$(OCAMLBUILD) -use-ocamlfind $@

clean:
	$(OCAMLBUILD) -clean

dist:
	mkdir $(DISTNAME)
	cp -r $(DISTFILES) $(DISTNAME)
	tar cvzf $(DISTNAME).tar.gz $(DISTNAME)
	rm -rf $(DISTNAME)

distclean: clean
	rm -f Makefile.config $(DISTNAME).tar.gz

fake-ocamlbuild:
	mkdir -p _build/
	for cmi in $(FAKE_CMI); do ln -sf $$($(OCAMLFIND) query compiler-libs.bytecomp)/$$cmi.cmi _build/ ; done


install: $(TARGET)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin
	install -dv $(SHARE_DIR)/ocamlmerlin/vim
	install emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	for file in vim/*; do install $$file $(SHARE_DIR)/ocamlmerlin/vim ; done
	@echo "Consult $(SHARE_DIR)/ocamlmerlin/vim/merlin.conf.vim to setup vim mode."

uninstall:
	rm -rf $(SHARE_DIR)/ocamlmerlin
