# ocaml-index

Ocaml-index is a tool that indexes values from CMT files. Its current purpose is
to provide project-wide occurrences for OCaml codebases. The tool iterate on
given cmt's occurrences list (`cmt_ident_occurrences`) and determines the
definition of every value found in it. It then write an index to disk where
values corresponding to the same definition are grouped together. The tool can
also take multiple input files, index them and merge the results into a single
index.

# Usage

## Process cmt files and merge indexes


> ocaml-index aggregate [-o _output_file_] _cmt_file_ ... _index_file_ ... [-I _dir_] ... [--no-cmt-load-path]


- Input `cmt` files are indexed and merged into the final output
- Input index files are directly merged into the output
- If no input files is provided an empty index is created
- The default output file name is `project.ocaml-index`

### Load path:
Identifying definitions while processing `cmt` files may require loading any of
the `cmt` files of every transitive dependency of the compilation unit. By
default the `cmt_load_path` of the first input `cmt` file will be used to search
for these other units. One can add more paths to the load path using the `-I`
option. Usage of the cmt's loadpath can be disabled using the
`--no-cmt-load-path` option.

### Paths:
By default, the paths stored in the cmt's locations are relative to the
directory where the compiler was called. for build systems that do not always
call the compiler from the same root folder it might be useful to rewrite these
paths.

Using the `--root <path>` option stores the given path in the output file.
Additionally, the ` --rewrite-root` option will prepend `root` to all paths in
indexed location.

[Note: this feature is not used in the reference Dune rules, it might evolve in
the future if needed]

## Querying indexes

The tool does not provide actual queries but one can dump an entire index:

> ocaml-index dump _index_file_ ...

Or only print the number of definitions it stores:

> ocaml-index stats _index_file_ ...

```bash
$ ocaml-index stats _build/default/src/dune_rules/.dune_rules.objs/cctx.ocaml-index
Index ".../cctx.ocaml-index" contains:
- 28083 definitions
- 86850 locations
- 0 approximated definitions
- 0 compilation units shapes
```
