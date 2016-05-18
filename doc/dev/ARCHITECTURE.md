Architecture
------------

### src/frontend

`ocamlmerlin.ml` is the entry point
It sets up the environment and enter a loop reading and dispatching commands.

`protocol.mli` defines all the type of all commands and their results

`IO.ml` implements conversion of commands to/and from JSON, using Yojson for producing and consuming strings

`IO_sexp.ml` takes JSON but produce and consume strings in an s-exp syntax

`command.ml` implements all command from Protocol. It manages state threaded through commands and setup appropriate buffer context, using functions from src/kernel and src/analysis.

Summary:
* manage communication with outside world
* dispatch commands, gather results

### src/kernel

Kernel wraps the OCaml frontend into an incremental and error-resilient library.

`merlin_lib.ml` implements Project and Buffer management.
Project represents data associated to a .merlin: file system paths, packages, flags, etc.
Buffer tracks knowledge of a specific buffer/file: tokens, syntax and type trees, dependencies on other modules, etc. Each Buffer belongs to one Project.

`dot_merlin.ml`: parsing and normalization of .merlin

`extension.ml`: list known and activated syntax extensions

`fake.ml`: generate fake pieces of AST that emulate semantics of camlp4 (and merlin-specific) extensions

`raw_lexer.ml`: incremental version of OCaml lexer

`merlin_lexer.ml`: wrapper over `Raw_lexer`

`merlin_parser.ml`: wrapper over `Raw_parser` (OCaml parser associated to a specific version)

`merlin_recover.ml`: extends `Merlin_parser` with recovery information

`merlin_recovery_explain.ml`: extract information from a parser explaining the context of syntax errors

`merlin_recovery_strategy.ml`: various analyses on LR-parser to help recover from incomplete parses

`merlin_browse.ml`: help navigation in typedtree, mainly answering "what is around this position?"

`merlin_typer.ml`: wrap OCaml typechecker

Summary:
* isolate state of OCaml frontend
* maintain multiple parsing and typing contexts in parallel
* robust to syntax and type errors

### src/analysis

Analysis offers different tools to work on the result of typechecking (produced by the kernel).
These are independent of an OCaml version, each typechecker comes with modules abstracting the differences.

`browseT.ml`: uniform traversal of typedtree, wrapping `Browse_node`

`browse_misc.ml`: tools too small to deserve their own module (tail calls annotations, printing ...)

`ocamldoc.ml`: get documentation associated to a definition

`typedtrie.ml`: quick lookup of OCaml paths

`type_utils.ml`: light wrapper over some functions of OCaml typer

`completion.ml`: generate list of completions

`expansion.ml`: like completion, but generate fuzzy/spell corrected suggestions rather than type-directed

`destruct.ml`: expand incomplete or coarse-grained patterns into more cases

`outline.ml`: produce an overview of an OCaml module's structure and definitions 

`track_definition.ml`: implement locate feature, i.e. "where is this entity defined?"

### src/ocaml (\_400, \_401, \_402, \_trunk)

Wraps a version of the OCaml typechecker for Merlin.
src/ocaml is a symlink to a concrete version selected at configure-time.

`typing/`, `parsing/`: typechecker from upstream OCaml, with merlin-specific patches

`browse_node.ml`: fold over Typedtree in a uniform way

`raw_parser.ml`: patched OCaml parser

`raw_parser_values.ml`: expose various information about structure of the parser, for introspection, recovery, ...

`raw_compat.ml`: compatibility layer to process typechecker output, masking differences between versions

`raw_typer.ml`: wrapper to invoke the typechecker, masking differences between versions

`tail_analysis.ml`: low-level functions for determining tail-call positions

`tast_helper.ml`: a few functions for manually producing pieces of typed AST 

### src/ocaml\_aux

Definitions useful to all versions of the typechecker.

`clflags.ml`: compiler flags, unified different versions

`tbl.ml`: generic table, stolen from compiler; should be moved to utils?

`cmi_cache.ml`, `cmt_cache.ml`: cache for *.cmi and *.cmt files

`parsing_aux.ml`: utils for parser, mainly warning and location management

`typing_aux.ml`: utils for typer, mainly capture of type errors and annotation of erroneous AST nodes

`error_report.ml`: uniform reporting of errors (either from OCaml frontend or other tools, e.g. findlib); should be moved to analysis?!

### src/utils

Miscellaneous types and functions.

`file_cache.ml`: generic caching infrastructure

`history.ml`: Variation over zippers

`logger.ml`: generic logging functions

`misc.ml`, `std`: standard library complements

`mymap.ml`: standard Map module with an extended interface

`menhirLib.ml`: patched menhir interpreter

`menhirUtils.ml`: functions for introspecting menhir parsers

`ppxsetup.ml`: keep track a list of ppx preprocessors with their flags

`terminfo.ml`: access terminal information, from upstream compiler
