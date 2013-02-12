include Makefile.config

TARGET = ocamlmerlin.native

DISTNAME = ocamlmerlin-0.1
DISTFILES = configure Makefile README _tags vim $(wildcard *.ml *.mli *.mly *.mll)

OCAMLBUILD=ocamlbuild -Is .,typing,parsing,utils
OCAMLFIND=ocamlfind

all: $(TARGET)

$(TARGET):
	$(OCAMLBUILD) -use-ocamlfind $@

.PHONY: $(TARGET) all clean dist distclean install uninstall

clean:
	$(OCAMLBUILD) -clean

dist:
	mkdir $(DISTNAME)
	cp -r $(DISTFILES) $(DISTNAME)
	tar cvzf $(DISTNAME).tar.gz $(DISTNAME)
	rm -rf $(DISTNAME)

distclean: clean
	rm -f Makefile.config $(DISTNAME).tar.gz

install: $(TARGET)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin
	install -dv $(SHARE_DIR)/ocamlmerlin/vim
	install emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	for file in vim/*; do \
	  if test -f $$file; then install $$file $(SHARE_DIR)/ocamlmerlin/vim ; \
	  else install -d $$file $(SHARE_DIR)/ocamlmerlin/vim;\
	  fi;\
	done
	@echo "Consult $(SHARE_DIR)/ocamlmerlin/vim/merlin.conf.vim to setup vim mode."

uninstall:
	rm -rf $(SHARE_DIR)/ocamlmerlin $(BIN_DIR)/ocamlmerlin $(SHARE_DIR)/emacs/site-lisp/merlin.el
