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

The expected location is 2:7 of foo.ml, but it instead goes to 1:9, which is the
constructor Foo
  $ $MERLIN single locate -position 4:28 -look-for ml \
  > -filename test1.ml < test1.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/foo.ml",
    "pos": {
      "line": 1,
      "col": 9
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
