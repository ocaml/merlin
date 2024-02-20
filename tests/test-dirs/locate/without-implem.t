  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >noimpl.mli <<EOF
  > type t = int
  > EOF

  $ cat >main.ml <<EOF
  > let _x : Noimpl.t = 42
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modules_without_implementation noimpl))
  > EOF

  $ dune build ./main.exe 2> /dev/null

  $ $MERLIN single locate -look-for ml -position 1:16 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/noimpl.mli",
      "pos": {
        "line": 1,
        "col": 5
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 1:16 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/noimpl.mli",
      "pos": {
        "line": 1,
        "col": 5
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 1:13 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/noimpl.mli",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for mli -position 1:13 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/noimpl.mli",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
