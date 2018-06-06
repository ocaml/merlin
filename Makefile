#### Main Makefile parameters

-include Makefile.config
TARGET = ocamlmerlin-server

EMACS_OBJECTS = emacs/merlin.elc \
							  emacs/merlin-iedit.elc \
							  emacs/merlin-imenu.elc \
							  emacs/merlin-ac.elc \
							  emacs/merlin-cap.elc \
							  emacs/merlin-company.elc \
							  emacs/merlin-xref.elc

ifeq ($(ENABLE_COMPILED_EMACS_MODE),true)
    TARGET_EMACS = $(EMACS_OBJECTS)
endif

EMACS = emacs

DESTDIR ?=
BIN_DIR := $(DESTDIR)$(BIN_DIR)
SHARE_DIR := $(DESTDIR)$(SHARE_DIR)
VIM_DIR := $(DESTDIR)$(VIM_DIR)

#### Invocation of OCamlMakefile

OCAMLMAKEFILE= $(MAKE) -f Makefile.ocamlmakefile \
							 WITHOUT_BIN_ANNOT="$(WITHOUT_BIN_ANNOT)" WITHOUT_DEBUG="$(WITHOUT_DEBUG)"

ifdef VERBOSE
	OCAMLMAKEFILE += REALLY_QUIET=0
endif

#### Default rule

all: $(TARGET) $(TARGET_EMACS) ocamlmerlin$(EXE)

#### Check configuration

CONFIG_FILES = src/config/my_config.ml src/ocaml/typer
$(CONFIG_FILES)$(MERLIN_OCAML_VERSION):
	@echo "Please run ./configure"
	@if [ -d ._d ]; then printf \
		"WARNING:\n\tThere are some build leftovers.\n\tConsider doing a 'make clean' before continuing.\n"; fi
	@false

assert_configured: $(CONFIG_FILES)$(MERLIN_OCAML_VERSION)

#### C wrapper

ocamlmerlin$(EXE): src/frontend/ocamlmerlin.c
	$(CC) $(if $(filter-out msvc,$(CCOMP_TYPE)),-o ,/nologo /Fe)$@ $^ $(if $(filter msvc,$(CCOMP_TYPE)),advapi32.lib)

#### Other rules

.PHONY: $(TARGET) all debug clean distclean install uninstall assert_configured message emacs-bytecode merlin.install

$(TARGET): assert_configured
	 +$(OCAMLMAKEFILE) $@

# this seems stale
test: assert_configured
	 +$(OCAMLMAKEFILE) PROJECT=test
	 ./ocamlmerlin-test$(EXE)

tests: $(TARGET)
	@$(MAKE) --no-print-directory -C tests

preprocess:
	$(MAKE) -f Makefile.preprocess

debug: assert_configured
	+$(OCAMLMAKEFILE) WITHOUT_BIN_ANNOT= WITHOUT_DEBUG= $(TARGET)

emacs-bytecode: $(EMACS_OBJECTS)

%.elc : %.el
	-$(EMACS) --batch --eval '(package-initialize)' -L emacs --no-init-file -f batch-byte-compile $<

clean:
	@rm -f Makefile.config $(CONFIG_FILES)
	@rm -f emacs/merlin.elc
	@rm -f src/ocaml/*/*/*.cmly
	$(MAKE) preprocessclean
	@find src/ -name '*.cm*' -delete
	@rm -f ocamlmerlin$(EXE) ocamlmerlin-test$(EXE)
	+$(OCAMLMAKEFILE) clean

distclean: clean
	@echo
	rm -f $(TARGET)$(EXE)

preprocessclean:
	$(MAKE) -f Makefile.preprocess clean

install-binary: $(TARGET) ocamlmerlin$(EXE)
	install -d $(BIN_DIR)
	install $(TARGET)$(EXE) $(BIN_DIR)/ocamlmerlin-server$(EXE)
	install ocamlmerlin$(EXE) $(BIN_DIR)/ocamlmerlin$(EXE)

install-share: $(TARGET_EMACS)
	install -d $(SHARE_DIR)
	install -d $(SHARE_DIR)/emacs/site-lisp
	install -m 644 emacs/merlin.el $(SHARE_DIR)/emacs/site-lisp/merlin.el
	test -f emacs/merlin.elc && install -m 644 emacs/merlin.elc $(SHARE_DIR)/emacs/site-lisp/merlin.elc || true
	install -m 644 emacs/merlin-iedit.el $(SHARE_DIR)/emacs/site-lisp/merlin-iedit.el
	-install -m 644 emacs/merlin-iedit.elc $(SHARE_DIR)/emacs/site-lisp/merlin-iedit.elc
	install -m 644 emacs/merlin-imenu.el $(SHARE_DIR)/emacs/site-lisp/merlin-imenu.el
	-install -m 644 emacs/merlin-imenu.elc $(SHARE_DIR)/emacs/site-lisp/merlin-imenu.elc
	install -m 644 emacs/merlin-ac.el $(SHARE_DIR)/emacs/site-lisp/merlin-ac.el
	-install -m 644 emacs/merlin-ac.elc $(SHARE_DIR)/emacs/site-lisp/merlin-ac.elc
	install -m 644 emacs/merlin-cap.el $(SHARE_DIR)/emacs/site-lisp/merlin-cap.el
	-install -m 644 emacs/merlin-cap.elc $(SHARE_DIR)/emacs/site-lisp/merlin-cap.elc
	install -m 644 emacs/merlin-company.el $(SHARE_DIR)/emacs/site-lisp/merlin-company.el
	-install -m 644 emacs/merlin-company.elc $(SHARE_DIR)/emacs/site-lisp/merlin-company.elc
	install -m 644 emacs/merlin-xref.el $(SHARE_DIR)/emacs/site-lisp/merlin-xref.el
	-install -m 644 emacs/merlin-xref.elc $(SHARE_DIR)/emacs/site-lisp/merlin-xref.elc

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
	@echo 'Take a look at https://github.com/ocaml/merlin for more information.'
	@echo

install: install-binary install-share install-vim
	+$(MAKE) message

merlin.install:
	@echo "Manually run 'sh merlin.install.sh' to update merlin.install"

uninstall:
	rm -rf $(SHARE_DIR)/ocamlmerlin$(EXE) \
				 $(BIN_DIR)/ocamlmerlin$(EXE)   \
				 $(BIN_DIR)/ocamlmerlin-server$(EXE)   \
				 $(SHARE_DIR)/emacs/site-lisp/merlin.el \
				 $(SHARE_DIR)/emacs/site-lisp/merlin.elc
