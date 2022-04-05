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
  $ rm prep.cm* prep.ml

Then our test files:

  $ mkdir liba libb

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ cat >liba/dune <<EOF
  > (library (name liba))
  > EOF

  $ cat >libb/dune <<EOF
  > (library
  >  (name libb)
  >  (preprocess (action (system "./prep.exe %{input-file}"))))
  > EOF

  $ cat >dune <<EOF
  > (library (name test) (libraries liba libb))
  > EOF

  $ cat >liba/dep.ml <<EOF
  > let x = "A"
  > EOF

  $ cat >libb/dep.ml <<EOF
  > let x = "B"
  > EOF

  $ cat >liba/liba.ml <<EOF
  > module Dep = Dep
  > EOF

  $ cat >libb/libb.ml <<EOF
  > module Dep = Dep
  > EOF

  $ cat >test.ml <<EOF
  > let _ = Liba.Dep.x
  > let _ = Libb.Dep.x
  > EOF

Now build with dune:

  $ BUILD_PATH_PREFIX_MAP= dune build 2>/dev/null

And confirm that locate works on both deps:

  $ $MERLIN single locate -look-for ml -position 1:15 \
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

  $ $MERLIN single locate -look-for ml -position 2:15 \
  > -filename test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/libb/dep.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
