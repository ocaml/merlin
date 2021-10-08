Setup the test environment:

  $ $OCAMLC -c -shapes -o Foo_test test.ml
  $ $OCAMLC -c -shapes foo.ml
  $ $OCAMLC -c -shapes bar.ml

Do the thing:

  $ echo "let () = Bar.the_function ()" | \
  > $MERLIN single locate -look-for ml -position 1:15 -filename ./example.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 5,
        "col": 4
      }
    },
    "notifications": []
  }
