#### Main Makefile parameters

-include Makefile.config
TARGET = ocamlmerlin

DESTDIR ?=
BIN_DIR := $(DESTDIR)$(BIN_DIR)
SHARE_DIR := $(DESTDIR)$(SHARE_DIR)

OCAML_VERSIONS = 401 402

#### Invocation of OCamlMakefile

OCAMLMAKEFILE= $(MAKE) -f Makefile.ocamlmakefile \
							 WITH_BIN_ANNOT="$(WITH_BIN_ANNOT)" WITH_DEBUG="$(WITH_DEBUG)"

ifndef VERBOSE
	OCAMLMAKEFILE += REALLY_QUIET=1
endif

#### Leftovers from previous buildsystem

OCAMLBUILD_LEFTOVERS = _build _tags src/config/myocamlbuild_config.ml ocamlmerlin.native

#### Default rule

all: $(TARGET)

#### Configuration

CONFIG_FILES = src/config/my_config.ml src/ocaml
$(CONFIG_FILES):
	@echo "Please run ./configure"
	@if [ -d ._d ]; then printf \
		"WARNING:\n\tThere are some build leftovers.\n\tConsider doing a 'make clean' before continuing.\n"; fi
	@false

#### Rules

.PHONY: $(TARGET) all debug clean distclean install uninstall assert_configured

assert_configured: $(CONFIG_FILES)

$(TARGET): assert_configured
	 +$(OCAMLMAKEFILE) $@

all_versions:
	for i in $(OCAML_VERSIONS); do \
		$(OCAMLMAKEFILE) OCAML_VERSION=_$$i; \
	done

wine:
	$(MAKE) -f Makefile.wine

preprocess:
	$(MAKE) -f Makefile.preprocess

preprocess_all_versions:
	for i in $(OCAML_VERSIONS); do \
		$(MAKE) -f Makefile.preprocess OCAML_VERSION=_$$i; \
	done

debug: assert_configured
	+$(OCAMLMAKEFILE) WITH_BIN_ANNOT=1 WITH_DEBUG=1 $(TARGET)

clean:
	@rm -f src/my_config.ml src/myocamlbuild_config.ml
	+$(OCAMLMAKEFILE) clean

check: $(TARGET)
	./test.sh

distclean: clean
	@echo
	rm -rf $(OCAMLBUILD_LEFTOVERS)
	rm -f Makefile.config $(CONFIG_FILES) $(TARGET)

install-binary: $(TARGET)
	install -d $(BIN_DIR)
	install $(TARGET) $(BIN_DIR)/ocamlmerlin
	install omake-merlin $(BIN_DIR)/omake-merlin
	install jenga-merlin $(BIN_DIR)/jenga-merlin

install-share: $(TARGET)
	install -d $(SHARE_DIR)
	install -d $(SHARE_DIR)/emacs/site-lisp
	install -m 644 emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el

install-vim: $(TARGET)
	install -d $(VIM_DIR)
	if [ -n "$(WITH_VIMBUFSYNC)" ]; then \
		cp -R vim/vimbufsync/* $(VIM_DIR)/; \
	fi
	cp -R vim/merlin/* $(VIM_DIR)

install: install-binary install-share install-vim
	@echo 
	@echo "Quick setup for VIM"
	@echo "-------------------"
	@echo "Add $(VIM_DIR) to your runtime path, e.g.:"
	@echo "  :set rtp+=$(VIM_DIR)"
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
	rm -rf $(SHARE_DIR)/ocamlmerlin \
				 $(BIN_DIR)/omake-merlin  \
				 $(BIN_DIR)/ocamlmerlin   \
				 $(SHARE_DIR)/emacs/site-lisp/merlin.el \
				 $(SHARE_DIR)/emacs/site-lisp/merlin.elc
