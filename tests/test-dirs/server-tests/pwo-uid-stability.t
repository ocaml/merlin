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

FIXME: duplicated occurrence
  $ $MERLIN server occurrences -identifier-at 3:4 \
  > -scope project -index-file .ocaml-index \
  > -log-file log -log-section occurrences \
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

  $ cat log 
  # 0.01 occurrences - occurrences
  Looking for occurences of z (pos: 3:4)
  # 0.01 occurrences - locs_of
  Cursor is on definition / declaration
  # 0.01 occurrences - occurrences
  Looking for uid of node pattern (lib.ml[3,12+4]..lib.ml[3,12+5])
    Tpat_var "z/277"
  # 0.01 occurrences - locs_of
  Definition has uid Lib.1 (File "lib.ml", line 3, characters 4-5)
  # 0.01 occurrences - locs_of
  Indexing current buffer
  # 0.01 occurrences - occurrences
  Found 2 locs
  # 0.01 occurrences - occurrences
  Found occ: z File "lib.ml", line 3, characters 4-5
  # 0.01 occurrences - occurrences
  Found occ: Lib File "main.ml", line 1, characters 9-14

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

FIXME: We are missing the occurrence in main.ml
  $ $MERLIN server occurrences -identifier-at 3:4 \
  > -scope project -index-file .ocaml-index \
  > -log-file log -log-section occurrences \
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
    }
  ]

  $ cat log 
  # 0.01 occurrences - occurrences
  Looking for occurences of z (pos: 3:4)
  # 0.01 occurrences - locs_of
  Cursor is on definition / declaration
  # 0.01 occurrences - occurrences
  Looking for uid of node pattern (lib.ml[3,22+4]..lib.ml[3,22+5])
    Tpat_var "z/279"
  # 0.01 occurrences - locs_of
  Definition has uid Lib.3 (File "lib.ml", line 3, characters 4-5)
  # 0.01 occurrences - locs_of
  Indexing current buffer
  # 0.01 occurrences - occurrences
  Found 0 locs

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

FIXME: we are missing the occurrence in main.ml 
  $ $MERLIN server occurrences -identifier-at 3:4 \
  > -scope project -index-file .ocaml-index \
  > -log-file log -log-section occurrences \
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
    }
  ]

  $ cat log
  # 0.01 occurrences - occurrences
  Looking for occurences of z (pos: 3:4)
  # 0.01 occurrences - locs_of
  Cursor is on definition / declaration
  # 0.01 occurrences - occurrences
  Looking for uid of node pattern (lib.ml[3,12+4]..lib.ml[3,12+5])
    Tpat_var "z/280"
  # 0.01 occurrences - locs_of
  Definition has uid Lib.4 (File "lib.ml", line 3, characters 4-5)
  # 0.01 occurrences - locs_of
  Indexing current buffer
  # 0.01 occurrences - occurrences
  Found 0 locs


  $ $MERLIN server stop-server
