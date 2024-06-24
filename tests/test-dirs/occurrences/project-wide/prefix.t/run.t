Compile a libary with prefixes, like dune

  $ cat >mylib.ml <<'EOF'
  > module A = Mylib__A
  > module B = Mylib__B
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -w -49 -no-alias-deps -o mylib.cmo -c -impl mylib.ml

  $ ocamlc -bin-annot -bin-annot-occurrences -open Mylib -o mylib__B.cmo -c b.ml

  $ ocamlc -bin-annot -bin-annot-occurrences -open Mylib -o mylib__A.cmo -c a.ml

Create an index

  $ ocaml-index aggregate *.cmt -o .merlin-index

  $ ocaml-index dump .merlin-index
  9 uids:
  {uid: Mylib__A; locs: "Mylib__A": File "mylib.ml", line 1, characters 11-19
   uid: Mylib__B; locs: "Mylib__B": File "mylib.ml", line 2, characters 11-19
   uid: Mylib.0; locs: "A": File "mylib.ml", line 1, characters 7-8
   uid: Mylib.1; locs: "B": File "mylib.ml", line 2, characters 7-8
   uid: Mylib__A.0; locs:
     "foo": File "a.ml", line 1, characters 4-7;
     "foo": File "a.ml", line 2, characters 10-13
   uid: Mylib__A.1; locs: "bar": File "a.ml", line 2, characters 4-7
   uid: Mylib__B.0; locs:
     "B.x": File "a.ml", line 1, characters 10-13;
     "B.x": File "a.ml", line 2, characters 16-19;
     "x": File "b.ml", line 1, characters 4-5;
     "x": File "b.ml", line 2, characters 8-9
   uid: Mylib__B.1; locs: "y": File "b.ml", line 2, characters 4-5
   uid: Stdlib.53; locs:
     "+": File "a.ml", line 2, characters 14-15;
     "+": File "b.ml", line 2, characters 10-11
   }, 0 approx shapes: {}, and shapes for CUS .

Merlin fails to find occurrences outside of file because of the module prefixes

  $ cat >.merlin <<'EOF'
  > INDEX .merlin-index
  > EOF

  $ $MERLIN single occurrences -scope project -identifier-at 1:4 -filename b.ml < b.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/b.ml",
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 5
        }
      },
      {
        "file": "$TESTCASE_ROOT/b.ml",
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      }
    ],
    "notifications": []
  }

Merlin successfully finds occurrences outside file when UNIT_NAME directive is used

  $ cat >.merlin <<'EOF'
  > INDEX .merlin-index
  > UNIT_NAME Mylib__B
  > EOF

  $ $MERLIN single occurrences -scope project -identifier-at 1:4 -filename b.ml < b.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/b.ml",
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 5
        }
      },
      {
        "file": "$TESTCASE_ROOT/a.ml",
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 13
        }
      },
      {
        "file": "$TESTCASE_ROOT/a.ml",
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 2,
          "col": 19
        }
      },
      {
        "file": "$TESTCASE_ROOT/b.ml",
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      }
    ],
    "notifications": []
  }

Merlin successfully finds occurrences outside file when WRAPPING_PREFIX directive is used

  $ cat >.merlin <<'EOF'
  > INDEX .merlin-index
  > WRAPPING_PREFIX Mylib__
  > EOF

  $ $MERLIN single occurrences -scope project -identifier-at 1:4 -filename b.ml < b.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/b.ml",
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 5
        }
      },
      {
        "file": "$TESTCASE_ROOT/a.ml",
        "start": {
          "line": 1,
          "col": 12
        },
        "end": {
          "line": 1,
          "col": 13
        }
      },
      {
        "file": "$TESTCASE_ROOT/a.ml",
        "start": {
          "line": 2,
          "col": 18
        },
        "end": {
          "line": 2,
          "col": 19
        }
      },
      {
        "file": "$TESTCASE_ROOT/b.ml",
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 9
        }
      }
    ],
    "notifications": []
  }
