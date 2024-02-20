
  $ $MERLIN single locate -look-for ml -position 4:11 -filename ./label.ml < ./label.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/label.ml",
      "pos": {
        "line": 3,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 9:11 -filename ./label.ml < ./label.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/label.ml",
      "pos": {
        "line": 8,
        "col": 4
      }
    },
    "notifications": []
  }

  $ cat >record.ml <<EOF
  > type t = { bar: int}
  > let foo = { bar = 42 }
  > let () = print_int foo.bar
  > EOF

  $ $MERLIN single locate -look-for mli -position 3:24 \
  > -filename ./record.ml < ./record.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/record.ml",
    "pos": {
      "line": 1,
      "col": 11
    }
  }

  $ $MERLIN single locate  -look-for ml -position 3:24 \
  > -filename ./record.ml < ./record.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/record.ml",
    "pos": {
      "line": 1,
      "col": 11
    }
  }

  $ cat >other_module.ml <<EOF
  > let foo = Record.{ bar = 42 }
  > let () = print_int foo.bar
  > EOF

  $ $OCAMLC -c -bin-annot record.ml

  $ $MERLIN single locate -look-for mli -position 2:24 \
  > -filename ./other_module.ml < ./other_module.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/record.ml",
    "pos": {
      "line": 1,
      "col": 11
    }
  }
