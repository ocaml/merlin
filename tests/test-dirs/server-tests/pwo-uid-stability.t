  $ $MERLIN server stop-server

  $ cat >lib.ml <<'EOF'
  > let x = ()
  > 
  > let z = ()
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = Lib.z
  > EOF

  $ $OCAMLC -c -bin-annot -bin-annot-occurrences lib.ml main.ml
  $ ocaml-index aggregate lib.cmt main.cmt -o .ocaml-index
  $ ocaml-index dump .ocaml-index
  2 uids:
  {uid: Lib.0; locs: "x": File "lib.ml", line 1, characters 4-5
   uid: Lib.1; locs:
     "z": File "lib.ml", line 3, characters 4-5;
     "Lib.z": File "main.ml", line 1, characters 9-14
   }, 0 approx shapes: {}, and shapes for CUS .


  $ $MERLIN server occurrences -identifier-at 3:4 \
  > -scope project -index-file .ocaml-index \
  > -filename lib.ml <lib.ml | jq '.value'
  [
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 3,
        "col": 4
      },
      "end": {
        "line": 3,
        "col": 5
      }
    },
    {
      "file": "$TESTCASE_ROOT/main.ml",
      "start": {
        "line": 1,
        "col": 13
      },
      "end": {
        "line": 1,
        "col": 14
      }
    }
  ]

Now we insert a def before z:
  $ cat >lib.ml <<'EOF'
  > let x = ()
  > let y = ()
  > let z = ()
  > EOF

  $ $OCAMLC -c -bin-annot -bin-annot-occurrences lib.ml main.ml
  $ ocaml-index aggregate lib.cmt main.cmt -o .ocaml-index
  $ ocaml-index dump .ocaml-index
  3 uids:
  {uid: Lib.0; locs: "x": File "lib.ml", line 1, characters 4-5
   uid: Lib.1; locs: "y": File "lib.ml", line 2, characters 4-5
   uid: Lib.2; locs:
     "z": File "lib.ml", line 3, characters 4-5;
     "Lib.z": File "main.ml", line 1, characters 9-14
   }, 0 approx shapes: {}, and shapes for CUS .

We are not missing the occurrence in main.ml
  $ $MERLIN server occurrences -identifier-at 3:4 \
  > -scope project -index-file .ocaml-index \
  > -filename lib.ml <lib.ml | jq '.value'
  [
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 3,
        "col": 4
      },
      "end": {
        "line": 3,
        "col": 5
      }
    },
    {
      "file": "$TESTCASE_ROOT/main.ml",
      "start": {
        "line": 1,
        "col": 13
      },
      "end": {
        "line": 1,
        "col": 14
      }
    }
  ]

  $ cat >lib.ml <<'EOF'
  > let x = ()
  > 
  > let z = ()
  > EOF

  $ $OCAMLC -c -bin-annot -bin-annot-occurrences lib.ml main.ml
  $ ocaml-index aggregate lib.cmt main.cmt -o .ocaml-index
  $ ocaml-index dump .ocaml-index
  2 uids:
  {uid: Lib.0; locs: "x": File "lib.ml", line 1, characters 4-5
   uid: Lib.1; locs:
     "z": File "lib.ml", line 3, characters 4-5;
     "Lib.z": File "main.ml", line 1, characters 9-14
   }, 0 approx shapes: {}, and shapes for CUS .

We are not missing the occurrence in main.ml 
  $ $MERLIN server occurrences -identifier-at 3:4 \
  > -scope project -index-file .ocaml-index \
  > -filename lib.ml <lib.ml | jq '.value'
  [
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 3,
        "col": 4
      },
      "end": {
        "line": 3,
        "col": 5
      }
    },
    {
      "file": "$TESTCASE_ROOT/main.ml",
      "start": {
        "line": 1,
        "col": 13
      },
      "end": {
        "line": 1,
        "col": 14
      }
    }
  ]

  $ $MERLIN server stop-server
