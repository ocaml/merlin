  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >mySet.mli <<EOF
  > module Make : functor (Arg : sig type t end) -> sig
  >   type t
  > end
  > EOF

  $ cat >mySet.ml <<EOF
  > module Make (Arg : sig type t end) = struct
  >   type t = Arg.t
  > end
  > EOF

  $ cat >s.ml <<EOF
  > module Foo = MySet.Make(struct
  >   type t
  > end)
  > type t = Foo.t
  > EOF

  $ cat >dune <<EOF
  > (executable (name s))
  > EOF

  $ dune build @check

Should jump to mySet.ml:
  $ $MERLIN single locate -look-for ml -position 4:13 \
  > -filename ./s.ml < ./s.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/mySet.ml",
    "pos": {
      "line": 2,
      "col": 7
    }
  }

Should jump to mySet.mli:
  $ $MERLIN single locate -look-for mli -position 4:13 \
  > -filename ./s.ml < ./s.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/mySet.mli",
    "pos": {
      "line": 2,
      "col": 7
    }
  }
