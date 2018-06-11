Create a well typed a.ml and a.mli and compile them
(this will generate a .cmi and .cmt):

  $ echo "type t = A | B" > a.ml
  $ echo "type t = A | B" > a.mli
  $ $OCAMLC -c a.mli
  $ $OCAMLC -c -bin-annot a.ml
  $ test -f a.cmi & test -f a.cmt & test ! -f a.cmti

Jump:

  $ $MERLIN single locate -look-for ml -position 1:11 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/partial-cmt/a.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

Remove the cmt:

  $ rm a.cmt

Introduce a type error in a.ml:

  $ echo "let () = 3" >> a.ml
  $ $OCAMLC -c -bin-annot a.ml
  File "a.ml", line 2, characters 9-10:
  Error: This expression has type int but an expression was expected of type
           unit
  @@ exit 2
  $ test -f a.cmi & test -f a.cmt & test ! -f a.cmti

Jump:

  $ $MERLIN single locate -look-for ml -position 1:11 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "tests/locate/partial-cmt/a.mli",
      "pos": {
        "line": 1,
        "col": 9
      }
    },
    "notifications": []
  }

