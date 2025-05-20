This test reproduces the issue described in:
https://github.com/ocaml/merlin/issues/1934


  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ mkdir lib
  $ cat >lib/lib.ml <<EOF
  > let message = "hello"
  > EOF
  $ cat >lib/dune <<EOF
  > (library
  >  (name lib)
  >  (preprocess (action (run sed "s/hello/world/g" %{input-file}))))
  > EOF

  $ mkdir lib2
  $ cat >lib2/lib.ml <<EOF
  > let message = "hello"
  > EOF
  $ cat >lib2/dune <<EOF
  > (library
  >  (name lib2)
  >  (preprocess (action (run sed "s/hello/world/g" %{input-file}))))
  > EOF


  $ cat >main.ml <<EOF
  > module M = Lib
  > let () = print_endline M.message
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries lib lib2))
  > EOF

  $ $DUNE exec ./main.exe
  world

  $ ls _build/default/lib/*.ml
  _build/default/lib/lib.ml
  _build/default/lib/lib.pp.ml

FIXME Merlin should treat Dune's .pp. files in a correct, ad-hoc way. Right it
appears that the digest of the original source file is not generated properly.
  $ $MERLIN single locate -look-for ml -position 1:12 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Several source files in your path have the same name, and merlin doesn't know which is the right one: $TESTCASE_ROOT/lib2/lib.ml, $TESTCASE_ROOT/lib/lib.ml",
    "notifications": []
  }
