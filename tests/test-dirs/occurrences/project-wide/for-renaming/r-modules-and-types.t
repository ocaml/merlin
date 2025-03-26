  $ cat >lib.mli <<'EOF'
  > module type S = sig
  >   val x : unit
  > end
  > EOF

  $ cat >lib.ml <<'EOF'
  > module type S = sig
  >   val x : unit
  > end
  > let () = 
  >   let module X : S = struct let x = () end in
  >   X.x
  > EOF

  $ cat >main.ml <<'EOF'
  > module M : Lib.S = struct
  >   let x = ()
  > end
  > module N = M
  > let () = let open M in N.x
  > EOF
 
  $ ocamlc -bin-annot -bin-annot-occurrences -c lib.mli lib.ml main.ml
  $ ocaml-index aggregate *.cmti *.cmt

  $ ocaml-index dump project.ocaml-index
  8 uids:
  {uid: [intf]Lib.0; locs: "x": File "lib.mli", line 2, characters 6-7
   uid: Lib.0; locs: "x": File "lib.ml", line 2, characters 6-7
   uid: [intf]Lib.1; locs: "S": File "lib.mli", line 1, characters 12-13
   uid: Lib.1; locs:
     "S": File "lib.ml", line 1, characters 12-13;
     "S": File "lib.ml", line 5, characters 17-18;
     "Lib.S": File "main.ml", line 1, characters 11-16
   uid: Lib.2; locs:
     "x": File "lib.ml", line 5, characters 32-33;
     "X.x": File "lib.ml", line 6, characters 2-5
   uid: Main.0; locs:
     "x": File "main.ml", line 2, characters 6-7;
     "N.x": File "main.ml", line 5, characters 23-26
   uid: Main.1; locs:
     "M": File "main.ml", line 1, characters 7-8;
     "M": File "main.ml", line 4, characters 11-12;
     "M": File "main.ml", line 5, characters 18-19
   uid: Main.2; locs: "N": File "main.ml", line 4, characters 7-8 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{([intf]Lib.1 Lib.1); ([intf]Lib.0 Lib.0 Lib.2 Main.0)}

  $ $MERLIN single occurrences -scope renaming -identifier-at 5:25 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 7
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/lib.ml",
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 7
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/lib.ml",
        "start": {
          "line": 5,
          "col": 32
        },
        "end": {
          "line": 5,
          "col": 33
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/lib.ml",
        "start": {
          "line": 6,
          "col": 4
        },
        "end": {
          "line": 6,
          "col": 5
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/lib.mli",
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 7
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 5,
          "col": 25
        },
        "end": {
          "line": 5,
          "col": 26
        },
        "stale": false
      }
    ],
    "notifications": []
  }

  $ $MERLIN single occurrences -scope renaming -identifier-at 1:7 \
  > -index-file project.ocaml-index \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 1,
          "col": 7
        },
        "end": {
          "line": 1,
          "col": 8
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 4,
          "col": 11
        },
        "end": {
          "line": 4,
          "col": 12
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 5,
          "col": 18
        },
        "end": {
          "line": 5,
          "col": 19
        },
        "stale": false
      }
    ],
    "notifications": []
  }
