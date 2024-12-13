Create a module with an mli file
  $ cat > foo.ml << EOF
  > type t = Foo
  > module Bar = struct
  >   type t = Bar
  > end
  > EOF

  $ cat > foo.mli << EOF
  > module Bar : sig
  >   type t
  > end
  > type t
  > EOF

  $ $OCAMLC -c -bin-annot foo.mli
  $ $OCAMLC -c -bin-annot foo.ml

Locate the Bar on line 4
  $ cat > test1.ml << EOF
  > module type Foo = sig
  >   include module type of Foo
  >   module Bar : sig
  >     include module type of Bar
  >   end
  > end
  > EOF


  $ $MERLIN single locate -position 4:28 -look-for mli \
  > -filename test1.ml < test1.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 1,
      "col": 7
    }
  }

Module Bar in foo.mli is a correct answer, but since there is only
one corresponding implementation we can jump there instead.
  $ $MERLIN single locate -position 4:28 -look-for ml \
  > -filename test1.ml < test1.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/foo.ml",
    "pos": {
      "line": 2,
      "col": 7
    }
  }

Locate the Bar on line 3
  $ cat > test2.ml << EOF
  > include Foo
  > module Bar = struct
  >   include Bar
  > end
  > EOF

Correctly returns 2:7
  $ $MERLIN single locate -position 3:12 -look-for ml -filename test2.ml < test2.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/foo.ml",
    "pos": {
      "line": 2,
      "col": 7
    }
  }

Locate the Foo.Bar on line 1
  $ cat > test3.ml << EOF
  > include module type of Foo.Bar
  > EOF
Correctly returns 2:7
  $ $MERLIN single locate -position 1:28 -look-for ml -filename test3.ml < test3.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/foo.ml",
    "pos": {
      "line": 2,
      "col": 7
    }
  }
