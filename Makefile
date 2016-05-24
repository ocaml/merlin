#### Main Makefile parameters

-include Makefile.config
TARGET = ocamlmerlin

ifdef ENABLE_COMPILED_EMACS_MODE
    TARGET_EMACS = emacs/merlin.elc \
									 emacs/merlin-iedit.elc \
									 emacs/merlin-ac.elc \
									 emacs/merlin-cap.elc \
									 emacs/merlin-company.elc \
									 emacs/merlin-compat.elc
endif

EMACS = emacs

DESTDIR ?=
BIN_DIR := $(DESTDIR)$(BIN_DIR)
SHARE_DIR := $(DESTDIR)$(SHARE_DIR)
VIM_DIR := $(DESTDIR)$(VIM_DIR)

OCAML_VERSIONS = 401 402

#### Invocation of OCamlMakefile

OCAMLMAKEFILE= $(MAKE) -f Makefile.ocamlmakefile \
							 WITH_BIN_ANNOT="$(WITH_BIN_ANNOT)" WITHOUT_DEBUG="$(WITHOUT_DEBUG)"

ifndef VERBOSE
	OCAMLMAKEFILE += REALLY_QUIET=1
endif

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
	$(MAKE) -f Makefile.wine installer

preprocess:
	$(MAKE) -f Makefile.preprocess

debug: assert_configured
	+$(OCAMLMAKEFILE) WITH_BIN_ANNOT=1 WITHOUT_DEBUG= $(TARGET)

%.elc : %.el
	-$(EMACS) --batch --no-init-file -f batch-byte-compile $<

clean:
	@rm -f Makefile.config $(CONFIG_FILES)
	@rm -f emacs/merlin.elc
	@rm -f src/ocaml_*/*.cmly
	$(MAKE) -f Makefile.preprocess clean
	@find src/ -name '*.cm*' -delete
	+$(OCAMLMAKEFILE) clean

check: $(TARGET)
	./test.sh

distclean: clean
	@echo
	rm -f $(TARGET)

install-binary: $(TARGET)
	install -d $(BIN_DIR)
	install $(TARGET)$(EXE) $(BIN_DIR)/ocamlmerlin$(EXE)

install-share: $(TARGET_EMACS)
	install -d $(SHARE_DIR)
	install -d $(SHARE_DIR)/emacs/site-lisp
	install -m 644 emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	test -f emacs/merlin.elc && install -m 644 emacs/merlin.elc $(SHARE_DIR)/emacs/site-lisp/merlin.elc || true
	install -m 644 emacs/merlin-iedit.el $(SHARE_DIR)/emacs/site-lisp/merlin-iedit.el
	-install -m 644 emacs/merlin-iedit.elc $(SHARE_DIR)/emacs/site-lisp/merlin-iedit.elc
	install -m 644 emacs/merlin-ac.el $(SHARE_DIR)/emacs/site-lisp/merlin-ac.el
	-install -m 644 emacs/merlin-ac.elc $(SHARE_DIR)/emacs/site-lisp/merlin-ac.elc
	install -m 644 emacs/merlin-cap.el $(SHARE_DIR)/emacs/site-lisp/merlin-cap.el
	-install -m 644 emacs/merlin-cap.elc $(SHARE_DIR)/emacs/site-lisp/merlin-cap.elc
	install -m 644 emacs/merlin-company.el $(SHARE_DIR)/emacs/site-lisp/merlin-company.el
	-install -m 644 emacs/merlin-company.elc $(SHARE_DIR)/emacs/site-lisp/merlin-company.elc
	install -m 644 emacs/merlin-compat.el $(SHARE_DIR)/emacs/site-lisp/merlin-compat.el
	-install -m 644 emacs/merlin-compat.elc $(SHARE_DIR)/emacs/site-lisp/merlin-compat.elc

install-vim:
	install -d $(VIM_DIR)
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
