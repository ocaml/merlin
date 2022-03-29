  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF

  $ cat >dune <<EOF
  > (executable (name test))
  > EOF

  $ cat >test.ml <<EOF
  > let _ = Test2.foo
  > EOF

  $ cat >test2.ml <<EOF
  > let foo = 42
  > EOF

  $ cat >test2.mli <<EOF
  > val foo : int
  > EOF

  $ dune build

Jump to interface:
  $ $MERLIN single locate -look-for mli -position 1:16 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test2.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

Jump to definition:
FIXME: it should jump to the ml file
  $ $MERLIN single locate -look-for ml -position 1:16 \
  > -filename test.ml <test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test2.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }
