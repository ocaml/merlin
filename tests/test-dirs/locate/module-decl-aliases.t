  $ cat >main.ml <<EOF
  > module Bar : sig 
  >   module Foo : sig
  >     val v : int
  >   end
  > end = struct
  >   module Foo = struct
  >     let v = 42
  >   end
  > end
  > module Foo = Bar.Foo
  > let _ = Foo.v
  > EOF

  $ cat>other.ml <<EOF
  > module Foo = Main.Bar.Foo
  > let _ = Foo.v
  > EOF

  $ $OCAMLC -c -bin-annot main.ml other.ml

  $ $MERLIN single locate -look-for ml -position 11:10 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 6,
      "col": 9
    }
  }

  $ $MERLIN single locate -look-for mli -position 11:10 \
  > -filename ./main.ml < ./main.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 2,
      "col": 9
    }
  }
  $ $MERLIN single locate -look-for ml -position 2:10 \
  > -filename ./other.ml < ./other.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 6,
      "col": 9
    }
  }

  $ $MERLIN single locate -look-for mli -position 2:10 \
  > -filename ./other.ml < ./other.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "pos": {
      "line": 2,
      "col": 9
    }
  }
