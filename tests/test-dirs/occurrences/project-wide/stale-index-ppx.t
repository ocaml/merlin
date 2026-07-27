First, prepare our preprocessor:

  $ cat >prep.ml <<EOF
  > let output_to_stdout fn str =
  >   output_string stdout Config.ast_impl_magic_number;
  >   output_value  stdout (fn : string);
  >   output_value  stdout (str : Parsetree.structure)
  > 
  > let () =
  >   let to_file, in_file =
  >     match Sys.argv.(1) with
  >     | "-dump-to-file" -> true, Sys.argv.(2)
  >     | filename -> false, filename
  >   in
  >   let str = Pparse.parse_implementation ~tool_name:"prep" in_file in
  >   if to_file then
  >     let out_file = Filename.chop_suffix in_file "ml" ^ "pp.ml" in
  >     Pparse.write_ast Structure out_file str
  >   else
  >     output_to_stdout in_file str
  > EOF

  $ $OCAMLC -I +compiler-libs ocamlcommon.cma -o prep.exe prep.ml

Then create the test project and compile it:

  $ cat >lib.ml <<'EOF'
  > (* blah *)
  > let foo = "bar"
  > EOF

  $ cat >main.ml <<'EOF'
  > let () = print_string Lib.foo
  > EOF

  $ ./prep.exe main.ml > main.pp.ml
  $ ./prep.exe lib.ml > lib.pp.ml

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c lib.pp.ml -o lib
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c main.pp.ml -o main

  $ ocaml-index aggregate main.cmt lib.cmt

Foo was defined on line 2 when the index was built, but is now defined on line 1
  $ cat >lib.ml <<'EOF'
  > let foo = "bar"
  > EOF

  $ $MERLIN single occurrences -scope project -identifier-at 1:28 \
  > -index-file project.ocaml-index \
  > -filename main.ml < main.ml | jq .value
  [
    {
      "file": "$TESTCASE_ROOT/main.ml",
      "start": {
        "line": 1,
        "col": 26
      },
      "end": {
        "line": 1,
        "col": 29
      },
      "stale": false
    },
    {
      "file": "$TESTCASE_ROOT/lib.ml",
      "start": {
        "line": 2,
        "col": 4
      },
      "end": {
        "line": 2,
        "col": 7
      },
      "stale": false
    }
  ]
