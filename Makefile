-include Makefile.config

TARGET = ocamlmerlin.native

OCAMLBUILD=ocamlbuild -Is src,src/typing,src/parsing,src/utils
OCAMLFIND=ocamlfind

all: $(TARGET)

CONFIG_FILES = src/my_config.ml src/myocamlbuild_config.ml
$(CONFIG_FILES):
	@echo "Please run ./configure"
	@false

assert_configured: $(CONFIG_FILES)

$(TARGET): assert_configured
	$(OCAMLBUILD) -use-ocamlfind $@

dev: assert_configured
	$(OCAMLBUILD) -cflags -bin-annot -use-ocamlfind $(TARGET)

debug: assert_configured
	$(OCAMLBUILD) -cflags -bin-annot -use-ocamlfind $(TARGET) -tag debug

.PHONY: $(TARGET) all dev clean distclean install uninstall assert_configured

clean:
	$(OCAMLBUILD) -clean

check: $(TARGET)
	./test.sh

distclean: clean
	@echo
	rm -f Makefile.config $(CONFIG_FILES) $(TARGET)

install: $(TARGET)
	install -d $(BIN_DIR)
	install -d $(SHARE_DIR)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin
	install omake-merlin $(BIN_DIR)/omake-merlin
	install jenga-merlin $(BIN_DIR)/jenga-merlin
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
	rm -rf $(SHARE_DIR)/ocamlmerlin $(BIN_DIR)/omake-merlin $(BIN_DIR)/ocamlmerlin $(SHARE_DIR)/emacs/site-lisp/merlin.el
