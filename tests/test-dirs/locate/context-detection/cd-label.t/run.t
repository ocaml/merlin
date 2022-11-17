
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

FIXME: this is not a very satisfying answer. 
We could expect 2:12 or at least 2:4
  $ $MERLIN single locate  -look-for ml -position 3:24 \
  > -filename ./record.ml < ./record.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/record.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

  $ cat >other_module.ml <<EOF
  > let foo = Record.{ bar = 42 }
  > let () = print_int foo.bar
  > EOF

  $ $OCAMLC -c -bin-annot record.ml

FIXME: Merlin looks for the Uid of the label in Record's `uid_to_loc` table but
doesn't find it. We need a compiler fix for this, see #1505.
  $ $MERLIN single locate -look-for mli -position 2:24 \
  > -filename ./other_module.ml < ./other_module.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/record.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }
