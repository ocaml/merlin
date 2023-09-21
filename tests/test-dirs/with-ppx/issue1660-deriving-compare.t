  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > module M = struct
  >   type t =
  >     | Foo
  >     | Bar
  > end
  > EOF

  $ $MERLIN single type-enclosing -position 2:7 \
  > -filename main.ml < main.ml |  jq '.value[0].type'
  "type t = Foo | Bar"


  $ cat >main.ml <<EOF
  > module M = struct
  >   type t =
  >     | Foo
  >     | Bar
  >   [@@deriving compare]
  > end
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name main)
  >  (preprocess (pps ppx_compare)))
  > EOF

  $ dune build

  $ $MERLIN single type-enclosing -position 2:7 \
  > -filename main.ml < main.ml |  jq '.value[0].type'
  "type t = Foo | Bar"
