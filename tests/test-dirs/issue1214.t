  $ cat > foo.ml <<EOF
  > module Make (X : sig end) = struct
  >   module N = struct end
  > end
  > EOF

  $ cat > bar.ml <<EOF
  > module M = struct end
  > open Foo.Make (M)
  > module O = N
  > EOF

  $ $OCAMLC -c -bin-annot foo.ml
  $ $OCAMLC -c -bin-annot bar.ml

This behaviour was described in the issue as failing but it works now.
It previously failing with : apply: "Needed cmti file of module 'N' to locate 'N' but it is not present".
  $ $MERLIN single locate -position 3:12 -filename bar.ml < bar.ml | jq .value | jq .pos
  {
    "line": 2,
    "col": 9
  }
