-include Makefile.config

TARGET = ocamlmerlin.native
#TARGET = src/spine.cmo

OCAMLBUILD=ocamlbuild -Is src,src/utils,src/ocaml$(TYPER_VERSION),src/ocaml$(TYPER_VERSION)/utils,src/ocaml$(TYPER_VERSION)/typing,src/ocaml$(TYPER_VERSION)/parsing
OCAMLFIND=ocamlfind

DESTDIR ?=
BIN_DIR := $(DESTDIR)$(BIN_DIR)
SHARE_DIR := $(DESTDIR)$(SHARE_DIR)

all: $(TARGET)

CONFIG_FILES = src/my_config.ml src/myocamlbuild_config.ml src/ocaml
$(CONFIG_FILES):
	@echo "Please run ./configure"
	@if [ -d _build ]; then printf \
		"WARNING:\n\t_build directory already exists.\n\tConsider doing a 'make clean' before continuing.\n"; fi
	@false

assert_configured: $(CONFIG_FILES)

$(TARGET): assert_configured
	$(OCAMLBUILD) $(INCLUDE_DEFAULT) $(WITH_BIN_ANNOT) -use-ocamlfind $@

all_versions:
	for i in _400 _401; do \
		$(MAKE) TYPER_VERSION=$$i $(TARGET);\
	 	cp $(TARGET) ocamlmerlin$$i;\
	done

debug: assert_configured
	$(OCAMLBUILD) -cflags -bin-annot -use-ocamlfind $(TARGET) -tag debug

.PHONY: $(TARGET) all dev clean distclean install uninstall assert_configured ocamlmerlin_400 ocamlmerlin_401

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
