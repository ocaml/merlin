#### Main Makefile parameters

-include Makefile.config
TARGET = ocamlmerlin

ifdef ENABLE_COMPILED_EMACS_MODE
    TARGET_EMACS = emacs/merlin.elc
endif

EMACS = emacs

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

all: $(TARGET) $(TARGET_EMACS)

#### Configuration

CONFIG_FILES = src/config/my_config.ml src/ocaml
$(CONFIG_FILES):
	@echo "Please run ./configure"
	@if [ -d ._d ]; then printf \
		"WARNING:\n\tThere are some build leftovers.\n\tConsider doing a 'make clean' before continuing.\n"; fi
	@false

#### Rules

.PHONY: $(TARGET) all debug clean distclean install uninstall assert_configured message merlin.install

assert_configured: $(CONFIG_FILES)

$(TARGET): assert_configured
	 +$(OCAMLMAKEFILE) $@

all_versions:
	for i in $(OCAML_VERSIONS); do \
	  $(OCAMLMAKEFILE) clean; \
		echo "# building for ocaml $$i"; \
		$(OCAMLMAKEFILE) MERLIN_OCAML_VERSION=_$$i; \
	done

wine:
	$(MAKE) -f Makefile.wine

preprocess:
	$(MAKE) -f Makefile.preprocess

preprocess_all_versions:
	for i in $(OCAML_VERSIONS); do \
		$(MAKE) -f Makefile.preprocess MERLIN_OCAML_VERSION=_$$i; \
	done

debug: assert_configured
	+$(OCAMLMAKEFILE) WITH_BIN_ANNOT=1 WITH_DEBUG=1 $(TARGET)

%.elc : %.el
	-$(EMACS) --batch --no-init-file -f batch-byte-compile $<

clean:
	@rm -f src/my_config.ml src/myocamlbuild_config.ml
	@rm -f emacs/merlin.elc
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

install-share: $(TARGET) $(TARGET_EMACS)
	install -d $(SHARE_DIR)
	install -d $(SHARE_DIR)/emacs/site-lisp
	install -m 644 emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	-install -m 644 emacs/merlin.elc $(SHARE_DIR)/emacs/site-lisp/merlin.elc
	install -m 644 emacs/merlin-iedit.el $(SHARE_DIR)/emacs/site-lisp/merlin-iedit.el

install-vim: $(TARGET)
	install -d $(VIM_DIR)
	if [ -n "$(WITH_VIMBUFSYNC)" ]; then \
		cp -R vim/vimbufsync/* $(VIM_DIR)/; \
	fi
	cp -R vim/merlin/* $(VIM_DIR)

message:
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
	@echo 'Take a look at https://github.com/the-lambda-church/merlin for more information.'
	@echo

install: install-binary install-share install-vim
	+$(MAKE) message

merlin.install:
	@echo "Manually run 'sh merlin.install.sh' to update merlin.install"

uninstall:
	rm -rf $(SHARE_DIR)/ocamlmerlin \
				 $(BIN_DIR)/ocamlmerlin   \
				 $(SHARE_DIR)/emacs/site-lisp/merlin.el \
				 $(SHARE_DIR)/emacs/site-lisp/merlin.elc
