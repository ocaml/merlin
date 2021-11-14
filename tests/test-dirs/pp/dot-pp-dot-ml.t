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

  $ mkdir -p _build
  $ cp prep.ml _build
  $ (cd _build; $OCAMLC -I +compiler-libs ocamlcommon.cma -o prep.exe prep.ml)

Then our test files:

  $ mkdir liba libb

  $ cat >liba/dep.ml <<EOF
  > let x = "A"
  > EOF

  $ cat >libb/dep.ml <<EOF
  > let x = "B"
  > EOF

  $ cat >test.ml <<EOF
  > let _ = LibaDep.x
  > let _ = LibbDep.x
  > EOF

----------------------------------------------------------

Build the files in _build as dune would, preprocessing liba in the "usual" way
and libb with dune cached way:

  $ cp -r liba libb test.ml _build

  $ cd _build/liba
  $ $OCAMLC -c -pp ../prep.exe -bin-annot -o libaDep.cmo dep.ml
  $ cd ..

  $ cd libb
  $ ../prep.exe -dump-to-file ./dep.ml
  $ $OCAMLC -c -bin-annot -o libbDep.cmo dep.pp.ml
  $ cd ..

  $ $OCAMLC -I liba -I libb -c test.ml
  $ cd ..

And confirm that locate works on both deps:

  $ $MERLIN single locate -look-for ml -position 1:11 \
  > -build-path _build/liba -source-path liba \
  > -build-path _build/libb -source-path libb \
  > -filename test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/liba/dep.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 2:11 \
  > -build-path _build/liba -source-path liba \
  > -build-path _build/libb -source-path libb \
  > -filename test.ml < ./test.ml
  {
    "class": "return",
    "value": "Several source files in your path have the same name, and merlin doesn't know which is the right one: $TESTCASE_ROOT/liba/dep.ml, $TESTCASE_ROOT/libb/dep.ml",
    "notifications": []
  }
