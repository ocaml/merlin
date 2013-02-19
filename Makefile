include Makefile.config

TARGET = ocamlmerlin.native

DISTNAME = ocamlmerlin-0.1
DISTFILES = configure Makefile README _tags vim emacs $(wildcard *.ml *.mli *.mly *.mll)

OCAMLBUILD=ocamlbuild -Is .,typing,parsing,utils
OCAMLFIND=ocamlfind

all: $(TARGET)

$(TARGET):
	$(OCAMLBUILD) -use-ocamlfind $@

.PHONY: $(TARGET) all clean dist distclean install uninstall

clean:
	$(OCAMLBUILD) -clean

check: $(TARGET)
	./test.sh

dist:
	mkdir $(DISTNAME)
	cp -r $(DISTFILES) $(DISTNAME)
	tar cvzf $(DISTNAME).tar.gz $(DISTNAME)
	rm -rf $(DISTNAME)

distclean: clean
	rm -f Makefile.config $(DISTNAME).tar.gz

install: $(TARGET)
	install -dv $(BIN_DIR)
	install -dv $(SHARE_DIR)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin
	install -dv $(SHARE_DIR)/ocamlmerlin/vim
	install -dv $(SHARE_DIR)/emacs/site-lisp
	install -m 644 emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	cp -R vim/* $(SHARE_DIR)/ocamlmerlin/vim/
	@echo "Consult $(SHARE_DIR)/ocamlmerlin/vim/plugin/merlin.vim to setup vim mode."

uninstall:
	rm -rf $(SHARE_DIR)/ocamlmerlin $(BIN_DIR)/ocamlmerlin $(SHARE_DIR)/emacs/site-lisp/merlin.el
