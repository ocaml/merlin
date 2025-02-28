  $ dune exec --verbose ./main.exe
  Shared cache: enabled-except-user-rules
  Shared cache location: /Users/ulysse/.cache/dune/db
  Workspace root:
  $TESTCASE_ROOT
  Dune context:
   { name = "default"
   ; kind = "default"
   ; profile = Dev
   ; merlin = true
   ; fdo_target_exe = None
   ; build_dir = In_build_dir "default"
   ; instrument_with = []
   }
  Running[1]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamldep.opt -modules -impl main.ml) > _build/default/.main.eobjs/dune__exe__Main.impl.d
  Running[2]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -49 -nopervasives -nostdlib -g -bin-annot -bin-annot-occurrences -I lib/.lib.objs/byte -no-alias-deps -opaque -o lib/.lib.objs/byte/lib.cmo -c -impl lib/lib.ml-gen)
  Running[3]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamldep.opt -modules -impl lib/a.ml) > _build/default/lib/.lib.objs/lib__A.impl.d
  Running[4]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamldep.opt -modules -impl a.ml) > _build/default/.main.eobjs/dune__exe__A.impl.d
  Running[5]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -49 -nopervasives -nostdlib -g -I lib/.lib.objs/byte -I lib/.lib.objs/native -intf-suffix .ml-gen -no-alias-deps -opaque -o lib/.lib.objs/native/lib.cmx -c -impl lib/lib.ml-gen)
  Running[6]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -bin-annot-occurrences -I lib/.lib.objs/byte -no-alias-deps -opaque -open Lib -o lib/.lib.objs/byte/lib__A.cmo -c -impl lib/a.ml)
  Running[7]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -49 -nopervasives -nostdlib -g -bin-annot -bin-annot-occurrences -I .main.eobjs/byte -no-alias-deps -opaque -o .main.eobjs/byte/dune__exe.cmo -c -impl .main.eobjs/dune__exe.ml-gen)
  Running[8]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamldep.opt -modules -intf main.mli) > _build/default/.main.eobjs/dune__exe__Main.intf.d
  Running[9]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -I lib/.lib.objs/byte -I lib/.lib.objs/native -intf-suffix .ml -no-alias-deps -opaque -open Lib -o lib/.lib.objs/native/lib__A.cmx -c -impl lib/a.ml)
  Running[10]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -49 -nopervasives -nostdlib -g -I .main.eobjs/byte -I .main.eobjs/native -intf-suffix .ml-gen -no-alias-deps -opaque -o .main.eobjs/native/dune__exe.cmx -c -impl .main.eobjs/dune__exe.ml-gen)
  Running[11]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -open Lib -g -bin-annot -bin-annot-occurrences -I .main.eobjs/byte -I lib/.lib.objs/byte -no-alias-deps -opaque -open Dune__exe -o .main.eobjs/byte/dune__exe__A.cmo -c -impl a.ml)
  Running[12]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlc.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -open Lib -g -bin-annot -bin-annot-occurrences -I .main.eobjs/byte -I lib/.lib.objs/byte -no-alias-deps -opaque -open Dune__exe -o .main.eobjs/byte/dune__exe__Main.cmi -c -intf main.mli)
  Running[13]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -a -o lib/lib.cmxa lib/.lib.objs/native/lib.cmx lib/.lib.objs/native/lib__A.cmx)
  Running[14]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -open Lib -g -I .main.eobjs/byte -I .main.eobjs/native -I lib/.lib.objs/byte -I lib/.lib.objs/native -intf-suffix .ml -no-alias-deps -opaque -open Dune__exe -o .main.eobjs/native/dune__exe__A.cmx -c -impl a.ml)
  Running[15]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -open Lib -g -I .main.eobjs/byte -I .main.eobjs/native -I lib/.lib.objs/byte -I lib/.lib.objs/native -intf-suffix .ml -no-alias-deps -opaque -open Dune__exe -o .main.eobjs/native/dune__exe__Main.cmx -c -impl main.ml)
  Running[16]: (cd _build/default && /Users/ulysse/tmp/merlin-503/_opam/bin/ocamlopt.opt -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -open Lib -g -o main.exe lib/lib.cmxa .main.eobjs/native/dune__exe.cmx .main.eobjs/native/dune__exe__A.cmx .main.eobjs/native/dune__exe__Main.cmx)
  test

FIXME: There should be no error.
  $ $MERLIN single errors -filename main.ml <main.ml | jq '.value'
  [
    {
      "start": {
        "line": 1,
        "col": 14
      },
      "end": {
        "line": 1,
        "col": 17
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound value A.x"
    }
  ]

$ FILE=$PWD/main.ml
$ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin 
((5:INDEX157:$TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)(5:INDEX159:$TESTCASE_ROOT/_build/default/lib/.lib.objs/cctx.ocaml-index)(6:STDLIB44:/Users/ulysse/tmp/merlin-503/_opam/lib/ocaml)(11:SOURCE_ROOT113:$TESTCASE_ROOT)(17:EXCLUDE_QUERY_DIR)(1:B145:$TESTCASE_ROOT/_build/default/.main.eobjs/byte)(1:B147:$TESTCASE_ROOT/_build/default/lib/.lib.objs/byte)(1:S113:$TESTCASE_ROOT)(1:S117:$TESTCASE_ROOT/lib)(3:FLG(5:-open9:Dune__exe))(3:FLG(2:-w45:@1..3@5..28@30..39@43@46..47@49..57@61..62-4016:-strict-sequence15:-strict-formats12:-short-paths10:-keep-locs5:-open3:Lib2:-g))(9:UNIT_NAME15:dune__exe__Main))

FIXME: Dune should communicate the -open Dune__exe flag after the others.
  $ $MERLIN single dump-configuration -filename main.ml <main.ml | \
  > jq '.value.merlin.flags_applied'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-open",
        "Dune__exe"
      ]
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-w",
        "@1..3@5..28@30..39@43@46..47@49..57@61..62-40",
        "-strict-sequence",
        "-strict-formats",
        "-short-paths",
        "-keep-locs",
        "-open",
        "Lib",
        "-g"
      ]
    }
  ]

Using a .merlin with the FLG flags in the correct order works:
  $ cat >.merlin <<'EOF'
  > B _build/default/.main.eobjs/byte
  > B _build/default/lib/.lib.objs/byte
  > FLG -open Lib
  > FLG -open Dune__exe
  > EOF

  $ $MERLIN single dump-configuration -filename main.ml <main.ml | \
  > jq '.value.merlin.flags_applied'
  [
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-open",
        "Lib"
      ]
    },
    {
      "workdir": "$TESTCASE_ROOT",
      "workval": [
        "-open",
        "Dune__exe"
      ]
    }
  ]

  $ $MERLIN single errors -filename main.ml <main.ml | jq '.value'
  []
