  $ mkdir bin
  $ cp ../../../../install/default/bin/ocamlmerlin bin/ocamlmerlin
  $ cp ../../../../install/default/bin/ocamlmerlin-server bin/ocamlmerlin-server

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > print_endline "Hello world"
  > EOF

  $ PATH=bin ocamlmerlin single dump-configuration \
  > -filename main.ml <main.ml >output

Not sure why merlin complains about an unknown flag here.
Fixme: we could provide a better error message
  $ cat output | jq '.value.merlin.failures'
  [
    "unknown flag main.ml",
    "flag -filename: error, Unix.Unix_error(Unix.ENOENT, \"create_process\", \"dune\")"
  ]

  $ cat >.merlin <<EOF
  > S .
  > EOF

  $ PATH=bin ocamlmerlin single dump-configuration \
  > -filename main.ml <main.ml >output

Not sure why merlin complains about an unknown flag here.
Fixme: we could provide a better error message
  $ cat output | jq '.value.merlin.failures'
  [
    "unknown flag main.ml",
    "flag -filename: error, Unix.Unix_error(Unix.ENOENT, \"create_process\", \"dot-merlin-reader\")"
  ]
