  $ cat >lib.ml <<'EOF'
  > module My_module = struct
  >  let variable = ()
  > end
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = Lib.My_module.variable
  > EOF
 
  $ ocamlc -bin-annot -bin-annot-occurrences -c lib.ml main.ml
  $ ocaml-index aggregate *.cmt

  $ ocaml-index dump project.ocaml-index
  3 uids:
  {uid: Lib; locs: "Lib": File "main.ml", line 1, characters 9-12
   uid: Lib.0; locs:
     "variable": File "lib.ml", line 2, characters 5-13;
     "Lib.My_module.variable": File "main.ml", line 1, characters 9-31
   uid: Lib.1; locs:
     "My_module": File "lib.ml", line 1, characters 7-16;
     "Lib.My_module": File "main.ml", line 1, characters 9-22
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{}


  $ $MERLIN single occurrences -scope renaming -identifier-at 1:20 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml | jq '.value[]'
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "start": {
      "line": 1,
      "col": 13
    },
    "end": {
      "line": 1,
      "col": 22
    },
    "stale": false
  }
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "start": {
      "line": 1,
      "col": 7
    },
    "end": {
      "line": 1,
      "col": 16
    },
    "stale": false
  }


  $ $MERLIN single occurrences -scope renaming -identifier-at 1:10 \
  > -index-file project.ocaml-index \
  > -filename lib.ml <lib.ml | jq '.value[]'
  {
    "file": "$TESTCASE_ROOT/lib.ml",
    "start": {
      "line": 1,
      "col": 7
    },
    "end": {
      "line": 1,
      "col": 16
    },
    "stale": false
  }
  {
    "file": "$TESTCASE_ROOT/main.ml",
    "start": {
      "line": 1,
      "col": 13
    },
    "end": {
      "line": 1,
      "col": 22
    },
    "stale": false
  }
