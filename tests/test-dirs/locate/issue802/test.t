Compile the various units as dune would:

  $ $OCAMLC -c -no-alias-deps -w -49 -bin-annot mylib__.ml
  $ $OCAMLC -c -no-alias-deps -bin-annot -open Mylib__ -o Mylib__Error error.ml
  $ $OCAMLC -c -no-alias-deps -bin-annot -open Mylib__ -o Mylib__A a.ml

Test jumping from a normal constructor:

  $ $MERLIN single locate -look-for ml -position 5:21 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": "Not in environment 'Constructor'",
    "notifications": []
  }

From an exception:

  $ $MERLIN single locate -look-for ml -position 3:21 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": "Not in environment 'MyError'",
    "notifications": []
  }

From an extension constructor:

  $ $MERLIN single locate -look-for ml -position 7:16 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": "Not in environment 'C1'",
    "notifications": []
  }

And from the extensible type name itself:

  $ $MERLIN single locate -look-for ml -position 7:10 -filename ./a.ml < ./a.ml
  {
    "class": "return",
    "value": "Not in environment 'ext'",
    "notifications": []
  }
