
  $ $DUNE build @ocaml-index

  $ $DUNE exec ./main.exe
  Hello world!

We expect 2 occurrences in func.ml, 1 in func.mli and 2 in main.ml
  $ $MERLIN single occurrences -scope renaming -identifier-at 4:18 \
  > -filename main.ml <main.ml | jq '.value[] | .file,.start'
  "$TESTCASE_ROOT/main.ml"
  {
    "line": 1,
    "col": 22
  }
  "$TESTCASE_ROOT/func.ml"
  {
    "line": 1,
    "col": 24
  }
  "$TESTCASE_ROOT/func.ml"
  {
    "line": 3,
    "col": 30
  }
  "$TESTCASE_ROOT/func.mli"
  {
    "line": 1,
    "col": 24
  }
  "$TESTCASE_ROOT/main.ml"
  {
    "line": 4,
    "col": 16
  }
