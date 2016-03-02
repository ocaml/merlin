SRC = ../src
TYPER = $(SRC)/ocaml
KERNEL = $(SRC)/kernel

## Settings

### Parser

PARSER_I = $(TYPER)/preprocess/parser_raw.mly
PARSER_O = $(TYPER)/parser_raw.ml \
					 $(TYPER)/parser_raw.mli \
					 $(TYPER)/parser_raw.cmly

### Lexer

LEXER_I = $(KERNEL)/preprocess/lexer_raw.mll
LEXER_O = $(KERNEL)/lexer_raw.ml

### Derived files

TARGETS =                    \
  $(PARSER_O) $(LEXER_O)     \
  $(TYPER)/parser_printer.ml \
  $(TYPER)/parser_recover.ml \
  $(TYPER)/parser_explain.ml

all: $(TARGETS)

## Rules

### Preprocessors

GEN_PRINTER = printer/gen_printer
GEN_RECOVER = recover/gen_recover
GEN_EXPLAIN = explain/gen_explain

$(GEN_RECOVER):
	$(MAKE) -C recover

$(GEN_PRINTER): $(GEN_PRINTER).ml
	$(MAKE) -C printer

$(GEN_EXPLAIN): $(GEN_EXPLAIN).ml
	$(MAKE) -C explain

### Parser

MENHIR_FLAGS = --infer --inspection --table --cmly \
       --ocamlc 'ocamlc.opt -I $(TYPER)/parsing -I $(SRC)/ocaml_aux -I $(KERNEL)'

$(PARSER_O): $(PARSER_I)
	menhir $(MENHIR_FLAGS) $(PARSER_I)
	mv $(subst $(TYPER),$(TYPER)/preprocess,$(PARSER_O)) $(TYPER)

### Lexer

$(LEXER_O): $(LEXER_I)
	cd ..; ocamllex -o $(subst ../,,$@) $(subst ../,,$<)

### Derivation

$(TYPER)/parser_printer.ml: $(TYPER)/parser_raw.cmly $(GEN_PRINTER)
	$(GEN_PRINTER) $< > $@
	
$(TYPER)/parser_recover.ml: $(TYPER)/parser_raw.cmly $(GEN_RECOVER)
	$(GEN_RECOVER) $< > $@

$(TYPER)/parser_explain.ml: $(TYPER)/parser_raw.cmly $(GEN_EXPLAIN)
	$(GEN_EXPLAIN) $< > $@

### Clean

clean:
	$(MAKE) -C printer clean
	$(MAKE) -C recover clean
	$(MAKE) -C explain clean

clear:
	rm -f $(TARGETS)
