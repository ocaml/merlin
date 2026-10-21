  $ $MERLIN server stop-server

  $ mkdir dep
  $ cat >.merlin <<EOF
  > B dep
  > EOF

  $ errors () {
  >   ocamlmerlin server errors -filename $1 < $1 |
  >     jq -r '.value[] | "\(.start.line):\(.start.col) \(.message | gsub("\\s+"; " "))"'
  > }

Two documents use a module that has not been built yet.
  $ echo 'let _y : int = Dep_appearing.z' > a.ml
  $ cp a.ml b.ml

  $ errors a.ml
  1:15 Unbound module Dep_appearing

  $ errors b.ml
  1:15 Unbound module Dep_appearing

The module is built. Bump the directory's mtime to defeat coarse mtime
granularity.
  $ echo 'val z : int' > dep/dep_appearing.mli
  $ $OCAMLC -c dep/dep_appearing.mli
  $ touch -t 203001010000 dep

The error should disappear from both documents.
  $ errors a.ml
  $ errors b.ml

Even after editing b.ml.
  $ echo 'let _z = ()' >> b.ml
  $ errors b.ml

For reference, single mode:
  $ ocamlmerlin single errors -filename b.ml < b.ml | jq -r '.value[].message'
  $ $MERLIN server stop-server
