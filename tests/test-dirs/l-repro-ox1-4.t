  $ cat >import.ml <<EOF
  > module Foo = struct
  >   type t
  > end
  > EOF

  $ $OCAMLC -c import.ml

  $ cat >main.ml <<EOF
  > open! Import
  > 
  > type 'a set
  > 
  > module Make_set (M : sig
  >     type t
  >   end) =
  > struct
  >   type t = M.t set
  > end
  > 
  > type t = Make_set(Foo).t
  > EOF

  $ cat >.merlin <<EOF
  > FLG -short-paths
  > B.
  > EOF

We expect "type t = Make_set(Import.Foo).t"
  $ $MERLIN single type-enclosing -position 12:0 \
  > -log-file log -log-section short-paths \
  > -filename main.ml <main.ml | jq '.value[0].type'
  "type t = Make_set(Foo).t"

