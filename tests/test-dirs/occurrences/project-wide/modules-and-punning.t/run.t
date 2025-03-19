  $ $DUNE build @ocaml-index

  $ ocaml-index dump _build/default/.main.eobjs/cctx.ocaml-index
  7 uids:
  {uid: Dune__exe__Lib; locs: "Lib": File "main.ml", line 3, characters 8-11
   uid: Dune__exe__Lib.0; locs: "t": File "lib.ml", line 2, characters 7-8
   uid: Dune__exe__Lib.1; locs:
     "label_label": File "lib.ml", line 2, characters 13-24;
     "B.label_label": File "lib.ml", line 5, characters 10-23;
     "Lib.B.label_label": File "main.ml", line 4, characters 6-23;
     "Lib.B.label_label": File "main.ml", line 5, characters 6-23
   uid: Dune__exe__Lib.2; locs:
     "B": File "lib.ml", line 1, characters 7-8;
     "Lib": File "main.ml", line 4, characters 6-9;
     "Lib": File "main.ml", line 5, characters 6-9
   uid: Dune__exe__Lib.3; locs:
     "v": File "lib.ml", line 5, characters 4-5;
     "Lib.v": File "main.ml", line 3, characters 8-13
   uid: Dune__exe__Main.0; locs:
     "label_label": File "main.ml", line 5, characters 36-47
   uid: Stdlib.139; locs: "ignore": File "main.ml", line 5, characters 29-35 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

  $ $MERLIN single occurrences -scope project -identifier-at 4:18 -filename main.ml < main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/lib.ml",
        "start": {
          "line": 2,
          "col": 13
        },
        "end": {
          "line": 2,
          "col": 24
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/lib.ml",
        "start": {
          "line": 5,
          "col": 12
        },
        "end": {
          "line": 5,
          "col": 23
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 4,
          "col": 12
        },
        "end": {
          "line": 4,
          "col": 23
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 5,
          "col": 12
        },
        "end": {
          "line": 5,
          "col": 23
        },
        "stale": false
      }
    ],
    "notifications": []
  }
