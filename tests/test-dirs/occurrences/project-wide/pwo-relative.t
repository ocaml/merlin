This test demonstrates project-wide occurrences resolving file paths relative to the
correct directory.

Create a small ocaml project
  $ mkdir main
  $ mkdir lib
  $ cat > lib/foo.ml << EOF
  > let bar = "bar"
  > let () = print_string bar
  > EOF

  $ cat > main/main.ml << EOF
  > let x = Foo.bar
  > let () = print_string Foo.bar
  > EOF

Compile project
  $ ( cd lib ; $OCAMLC -bin-annot -bin-annot-occurrences -c foo.ml )
  $ ( cd main ; $OCAMLC -bin-annot -bin-annot-occurrences -I ../lib -c main.ml )

Build indices
  $ ( cd lib ; ocaml-index aggregate foo.cmt --root ./lib --rewrite-root -o lib.stanza.merlin-index )
  $ ( cd main ; ocaml-index aggregate main.cmt --root ./main --rewrite-root -o main.stanza.merlin-index )
  $ ocaml-index aggregate main/main.stanza.merlin-index lib/lib.stanza.merlin-index --root . --rewrite-root -o global.merlin-index
  $ ocaml-index dump global.merlin-index
  3 uids:
  {uid: Foo.0; locs:
     "bar": File "./lib/foo.ml", line 1, characters 4-7;
     "bar": File "./lib/foo.ml", line 2, characters 22-25;
     "Foo.bar": File "./main/main.ml", line 1, characters 8-15;
     "Foo.bar": File "./main/main.ml", line 2, characters 22-29
   uid: Main.0; locs: "x": File "./main/main.ml", line 1, characters 4-5
   uid: Stdlib.312; locs:
     "print_string": File "./lib/foo.ml", line 2, characters 9-21;
     "print_string": File "./main/main.ml", line 2, characters 9-21
   }, 0 approx shapes: {}, and shapes for CUS .

Configure merlin
  $ cat > main/.merlin << EOF
  > INDEX ../global.merlin-index
  > SOURCE_ROOT ..
  > S .
  > B ../lib
  > S ../lib
  > EOF

Perform the occurrences query
  $ ( cd main ; $MERLIN single occurrences -scope project -identifier-at 1:13 \
  > -filename main.ml < main.ml | jq .value )
  [
    {
      "file": "$TESTCASE_ROOT/lib/foo.ml",
      "start": {
        "line": 1,
        "col": 4
      },
      "end": {
        "line": 1,
        "col": 7
      }
    },
    {
      "file": "$TESTCASE_ROOT/lib/foo.ml",
      "start": {
        "line": 2,
        "col": 22
      },
      "end": {
        "line": 2,
        "col": 25
      }
    },
    {
      "file": "$TESTCASE_ROOT/main/main.ml",
      "start": {
        "line": 1,
        "col": 12
      },
      "end": {
        "line": 1,
        "col": 15
      }
    },
    {
      "file": "$TESTCASE_ROOT/main/main.ml",
      "start": {
        "line": 2,
        "col": 26
      },
      "end": {
        "line": 2,
        "col": 29
      }
    }
  ]
