Architecture
------------

### src/frontend

`ocamlmerlin.c` implements the `ocamlmerlin` wrapper that takes care of
spawning a server if necessary and passing queries. See [SERVER.md](SERVER.md).

`ocamlmerlin_server.ml` is the entry point of `ocamlmerlin-server` binary. It
reads argument from the command-line and decides which mode to start
(old-protocol, single query or query server).

`query_protocol.mli` defines the type of all queries and their results.

`query_json.ml` implements conversion of results to a JSON-like type. Yojson or
Sexp can be used for turning these into strings.

`query_commands.ml` executes the queries defined by the protocol. It uses
`src/kernel` for parse and typing. Then it uses `src/analysis` to get some
results.

`new/` implements a UNIX-like frontend: parametrized by arguments, reading
content from stdin, outputting answer to stdout and logging to stderr.
Command-line is turned into a `Query_protocol.t` query and executed with
`Query_commands`.

`old/` does the same job but for the previous, synchronous, version of the
protocol (see [OLD-PROTOCOL.ml](OLD-PROTOCOL.md)).

`test/ocamlmerlin_test.ml` implements the `ocamlmerlin-test` binary that runs a
testsuite of queries. 

Summary:
* manage communication with outside world
* get a query from the user by some mean, turn it into a `Query_protocol.t`
* execute the query with `Query_commands`
* return the result by some mean

### src/kernel

Kernel wraps the OCaml frontend into an incremental and error-resilient
library.

`mconfig.ml` defines a big record that contains all settings affecting Merlin
behavior, as well as a setting parser and dumper.

`mconfig_dot.ml` is used by `Mconfig` to process `.merlin` files

`mocaml.ml` interfaces with the OCaml typechecker. It setup and restore state
when entering/exiting the typechecker.

`mpipeline.ml` implements a few high-level primitives that connect all pieces
together

`mppx.ml`: implements ppx rewriting, directed by an `Mconfig.t`

`msource.ml`: represents is the representation of a source file in Merlin. It
also computes the positions of contents in the source file. 

`mreader.ml`: abstracts the parser of Merlin. It turns an `Msource.t` into an
AST. `mreader_*` implement a parser for normal OCaml files. 

`mreader_extent.ml`: a parser that delegates the work to (compatible) external
commands. Mainly used by Reason. See `src/extend/`.

`mreader_lexer.ml`, `mreader_parser.ml`, `mreader_recover.ml`,
`mreader_explain.ml`: a parser for standard OCaml files. Main addition is
recovery from syntax errors.

`mbrowse.ml`: uniform navigation in typedtree, mainly answering "what is around
this position?"

`mtyper.ml`: wraps the OCaml typechecker, to type the ASTs produced by
`mreader.ml`

`extension.ml`: defines some OCaml dialects (lwt camlp4 and meta-ocaml)

Summary:
* isolate state of OCaml typechecker
* maintain multiple parsing and typing contexts in parallel
* robust to syntax and type errors

### src/analysis

Analysis offers different tools to work with the result of typechecking
(produced by the kernel).
These are independent of an OCaml version. A typechecker comes with modules to
abstract the differences.

`browse_misc.ml`: tools too small to deserve their own module (tail calls
annotations, printing ...)

`browse_tree.ml`: uniform traversal of typedtree, wrapping `Browse_raw`

`ocamldoc.ml`: get documentation associated to a definition

`typedtrie.ml`: a trie representation of a compilation unit, allowing quick lookup of OCaml paths

`type_utils.ml`: light wrapper over some functions of OCaml typer

`completion.ml`: generate list of completions

`expansion.ml`: like completion, but generate fuzzy/spell corrected suggestions rather than type-directed

`destruct.ml`: expand incomplete or coarse-grained patterns into more cases

`outline.ml`: produce an overview of an OCaml module's structure and definitions 

`locate.ml`: implement a jump-to-definition/declaration feature

`jump.ml`: implement convenient nagivation commands

### src/ocaml/support

Definitions useful to all versions of the typechecker.

`clflags.ml`: compiler flags, unified between all versions

`tbl.ml`, `identifiable.ml`: needed by all OCaml typecheckers

`cmi_cache.ml`, `cmt_cache.ml`: cache for \*.cmi and \*.cmt files

`fake.ml`: generate fake pieces of AST that implement the semantics of
extensions from `src/kernel/extensions.ml`

`msupport.ml`: bridge between extensions to OCaml typecheker and Merlin kernel.
Mainly for warning and location management, capture of type errors and
annotation of erroneous AST nodes

`location_aux.ml`: small functions missing from location.ml (management of
character ranges)

`path_aux.ml`: small functions missing from path.ml (management of qualified
identifiers)

`preprocess/lexer_ident.mll`: a subset of the OCaml lexer to find identifiers
in the middle of arbitrary text.

### src/ocaml/typer (\_402, \_403, \_404)

Wraps a version of the OCaml typechecker for Merlin.
src/ocaml is a symlink to a concrete version selected at configure-time.

`typing/`, `parsing/`: typechecker from upstream OCaml, with merlin-specific patches

`browse_raw.ml`: fold over Typedtree in a uniform way

`parser_raw.ml`, `parser_recover.ml`, `parser_explain.ml`, `parser_recover.ml`:
an OCaml parser with extra information for recovery, produced by menhir and
with custom preprocessors (see `preprocessors/`)

`preprocess/lexer_raw.mll`: OCaml lexer, to be processed by `ocamllex`

`printf_compat.ml`: fix a too restrictive signature of OCaml 4.02, empty in
later versions

`raw_compat.ml`: compatibility layer to process typechecker output, masking differences between versions

`typer_raw.ml`: wrapper to invoke the typechecker, masking differences between versions

`tail_analysis.ml`: low-level functions for determining tail-call positions

`tast_helper.ml`: a few functions for manually producing pieces of typed AST 

### src/platform

Modules and stub to deal with platform specific features.

`fs_case.c`: primitive to handle case insensitivity of macOS

`os_ipc_stub.c`, `os_ipc.ml`: implements UNIX Domain Socket IPC for
`ocamlmerlin-server`

### src/sturgeon (\_null, \_stub)

Abstraction of [sturgeon](https://github.com/let-def/sturgeon) UI.

### src/utils

Miscellaneous types and functions.

`file_cache.ml`: generic caching infrastructure

`local_store.ml`: snapshot and restore mutable state 

`logger.ml`: generic logging functions

`marg.ml`: generic commandline-like argument parsing (merlin gets arguments
from different places)

`misc.ml`, `std.ml`: standard library complements

`menhirLib.ml`: patched menhir interpreter

`ppxsetup.ml`: keep track of ppx preprocessors with their flags

`sexp.ml`: a s-expression reader/writer

`trace.ml`: log information structured as a trace (with enter and exit of
sub-routines)
