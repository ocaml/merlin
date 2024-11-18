  $ cat >lib.mli <<'EOF'
  > module type S = sig
  >   val x : unit
  > end
  > EOF

  $ cat >lib.ml <<'EOF'
  > module type S = sig
  >   val x : unit
  > end
  > EOF

  $ cat >main.ml <<'EOF'
  > module M : Lib.S = struct
  >   let x = ()
  > end
  > let () = M.x
  > EOF
 
  $ ocamlc -bin-annot -bin-annot-occurrences -c lib.mli lib.ml main.ml
  $ ocaml-index aggregate *.cmti *.cmt

  $ ocaml-index dump project.ocaml-index
  6 uids:
  {uid: [intf]Lib.0; locs: "x": File "lib.mli", line 2, characters 6-7
   uid: Lib.0; locs: "x": File "lib.ml", line 2, characters 6-7
   uid: [intf]Lib.1; locs: "S": File "lib.mli", line 1, characters 12-13
   uid: Lib.1; locs:
     "S": File "lib.ml", line 1, characters 12-13;
     "Lib.S": File "main.ml", line 1, characters 11-16
   uid: Main.0; locs:
     "x": File "main.ml", line 2, characters 6-7;
     "M.x": File "main.ml", line 4, characters 9-12
   uid: Main.1; locs: "M": File "main.ml", line 1, characters 7-8 },
  0 approx shapes: {}, and shapes for CUS .
  and related uids:{([intf]Lib.1 Lib.1); ([intf]Lib.0 Lib.0 Main.0)}

  $ $MERLIN single occurrences -scope renaming -identifier-at 4:11 \
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
        }
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
        }
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
        }
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
        }
      }
    ],
    "notifications": []
  }

