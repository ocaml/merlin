-include Makefile.config

TARGET = ocamlmerlin.native

DISTNAME = ocamlmerlin-0.1
DISTFILES = configure Makefile README _tags vim emacs $(wildcard *.ml *.mli *.mly *.mll)

OCAMLBUILD=ocamlbuild -Is src,src/typing,src/parsing,src/utils
OCAMLFIND=ocamlfind

all: $(TARGET)

src/myocamlbuild_config.ml:
	@echo "Please run ./configure"
	@false

$(TARGET): src/myocamlbuild_config.ml
	$(OCAMLBUILD) -use-ocamlfind $@

.PHONY: $(TARGET) all clean distclean install uninstall

clean:
	$(OCAMLBUILD) -clean

check: $(TARGET)
	./test.sh

distclean: clean
	rm -f Makefile.config src/myocamlbuild_config.ml

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
