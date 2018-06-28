FIXME: this should say "Not in environment 'b'"

  $ $MERLIN single locate -look-for ml -position 2:10 -filename test.ml <<EOF \
  > let b = 10 \
  > let x = { b = 9 } \
  > EOF
  {
    "class": "return",
    "value": {
      "file": "test.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }
