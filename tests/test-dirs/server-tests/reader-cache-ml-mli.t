The reader phase cache (`Mpipeline.Reader_with_cache`, enabled by
`USE_PPX_CACHE`) is a single process-global entry whose fingerprint is the
source digest and the completion position. The filename, and hence whether the
buffer is parsed as an implementation or an interface, is not part of it. An
.mli with the same contents as a just-queried .ml is handed the .ml's
implementation parsetree.

  $ $MERLIN server stop-server

  $ cat >.merlin <<EOF
  > USE_PPX_CACHE
  > EOF

  $ errors () {
  >   ocamlmerlin server errors -filename $1 < $1 |
  >     jq -r '.value[] | "\(.start.line):\(.start.col) \(.message | gsub("\\s+"; " "))"'
  > }

A valid implementation, which is a syntax error as an interface.
  $ echo 'let _x : unit = ()' > same.ml
  $ cp same.ml same.mli
  $ errors same.ml

The interface should have a syntax error.
  $ errors same.mli
  1:0 Syntax error

Any change to the text misses the cache.
  $ echo '' >> same.mli
  $ errors same.mli
  1:0 Syntax error

For reference, single mode:
  $ cp same.ml same.mli
  $ ocamlmerlin single errors -filename same.mli < same.mli | jq -r '.value[].message'
  Syntax error
  $ $MERLIN server stop-server
