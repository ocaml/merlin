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

dev: src/myocamlbuild_config.ml
	$(OCAMLBUILD) -cflags -bin-annot -use-ocamlfind $(TARGET)

.PHONY: $(TARGET) all dev clean distclean install uninstall

clean:
	$(OCAMLBUILD) -clean

check: $(TARGET)
	./test.sh

distclean: clean
	rm -f Makefile.config src/myocamlbuild_config.ml

install: $(TARGET)
	install -d $(BIN_DIR)
	install -d $(SHARE_DIR)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin
	install -d $(SHARE_DIR)/ocamlmerlin/vim
	install -d $(SHARE_DIR)/ocamlmerlin/vimbufsync
	install -d $(SHARE_DIR)/emacs/site-lisp
	install -m 644 emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	cp -R vim/merlin/* $(SHARE_DIR)/ocamlmerlin/vim/
	cp -R vim/vimbufsync/* $(SHARE_DIR)/ocamlmerlin/vimbufsync/
	@echo 
	@echo "Quick setup for VIM"
	@echo "-------------------"
	@echo "Add $(SHARE_DIR)/ocamlmerlin/vim and vimbufsync to your runtime path, e.g.:"
	@echo "  :set rtp+=$(SHARE_DIR)/ocamlmerlin/vim"
	@echo "  :set rtp+=$(SHARE_DIR)/ocamlmerlin/vimbufsync"
	@echo 
	@echo "Quick setup for EMACS"
	@echo "-------------------"
	@echo "Add $(SHARE_DIR)/emacs/site-lisp to your runtime path, e.g.:"
	@echo '  (add-to-list '"'"'load-path "$(SHARE_DIR)/emacs/site-lisp")'
	@echo '  (require '"'"'merlin)'
	@echo 'Then issue M-x merlin-mode in a ML buffer.'
	@echo 
	@echo 'Take a look at https://github.com/def-lkb/merlin for more information.'
	@echo 

uninstall:
	rm -rf $(SHARE_DIR)/ocamlmerlin $(BIN_DIR)/ocamlmerlin $(SHARE_DIR)/emacs/site-lisp/merlin.el
