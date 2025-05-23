  $ cat >dune-project <<'EOF'
  > (lang dune 2.9)
  > EOF

  $ cat >main.ml <<'EOF'
  > open Lib
  > open Aux
  > let () = print_string foo
  > EOF

  $ cat >dune <<'EOF'
  > (executable (name main) (libraries lib))
  > EOF

  $ mkdir lib
  $ cd lib

  $ cat >aux.ml <<'EOF'
  > let foo = "bar"
  > let _ = foo
  > EOF

  $ cat >dune <<'EOF'
  > (library (name lib))
  > EOF

  $ cd ..
  $ dune exec ./main.exe
  bar

  $ dune build @ocaml-index

  $ cat _build/default/lib/lib.ml-gen
  (* generated by dune *)
  
  (** @canonical Lib.Aux *)
  module Aux = Lib__Aux

We should not index generated modules (lib.ml-gen)
  $ ocaml-index dump _build/default/lib/.lib.objs/cctx.ocaml-index
  1 uids:
  {uid: Lib__Aux.0; locs:
     "foo": File "lib/aux.ml", line 1, characters 4-7;
     "foo": File "lib/aux.ml", line 2, characters 8-11
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

  $ ocaml-index dump _build/default/.main.eobjs/cctx.ocaml-index
  4 uids:
  {uid: Lib; locs: "Lib": File "main.ml", line 1, characters 5-8
   uid: Lib__Aux; locs: "Aux": File "main.ml", line 2, characters 5-8
   uid: Lib__Aux.0; locs: "foo": File "main.ml", line 3, characters 22-25
   uid: Stdlib.312; locs:
     "print_string": File "main.ml", line 3, characters 9-21
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{}

  $ $MERLIN single occurrences -scope project -identifier-at 3:23 \
  > -filename main.ml <main.ml | jq '.value[].file'
  "$TESTCASE_ROOT/main.ml"
  "$TESTCASE_ROOT/lib/aux.ml"
  "$TESTCASE_ROOT/lib/aux.ml"
