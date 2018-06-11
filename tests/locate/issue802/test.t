Compile the various units as dune would:

  $ $OCAMLC -c -no-alias-deps -w -49 -bin-annot mylib__.ml
  $ $OCAMLC -c -no-alias-deps -bin-annot -open Mylib__ -o Mylib__Error error.ml
  $ $OCAMLC -c -no-alias-deps -bin-annot -open Mylib__ -o Mylib__A a.ml

Test jumping from a normal constructor:

  $ $MERLIN single locate -look-for ml -position 5:21 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/issue802/error.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

And from an exception (FIXME):

  $ $MERLIN single locate -look-for ml -position 3:21 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/issue802/mylib__.ml",
      "pos": {
        "line": 2,
        "col": 0
      }
    },
    "notifications": []
  }

