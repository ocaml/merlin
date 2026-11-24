  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat >main.ml <<EOF
  > module Let_syntax = struct
  >   let map x ~f = f x
  > end
  > 
  > let _ =
  >   let%map (x) = (5) in (x)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps ppx_let)))
  > EOF

  $ dune build @check

  $ LOC=6:11
  $ show_location main.ml $LOC
  module Let_syntax = struct
    let map x ~f = f x
  end
  
  let _ =
    let%map (█) = (5) in (x)

  $ $MERLIN single expand-prefix -prefix x -position $LOC -filename main.ml <main.ml
  Fatal error: exception Invalid_argument("Ppxlib.Longident.parse(empty string): \"\"")
  Raised at Ppxlib__Common.exn_to_loc_error in file "src/common.ml", line 259, characters 69-78
  Called from Ppxlib__Driver.exn_to_extension in file "src/driver.ml" (inlined), line 618, characters 2-22
  Called from Ppxlib__Driver.rewrite_binary_ast_file in file "src/driver.ml", line 1505, characters 16-66
  Called from Ppxlib__Driver.run_as_ppx_rewriter_main in file "src/driver.ml", line 1531, characters 6-48
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1565, characters 9-42
  Re-raised at Location.report_exception.loop in file "parsing/location.ml", line 1029, characters 14-25
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1569, characters 4-61
  Called from Dune__exe___ppx in file ".ppx/2d6b2d23d99d90ea0d96ce936dd9dd0e/_ppx.ml-gen", line 1, characters 9-36
  {
    "class": "return",
    "value": {
      "entries": [],
      "context": null
    },
    "notifications": []
  }

  $ cat >opt_arg.ml <<EOF
  > module S = struct module Key = struct type t end end
  > let lcas t ?(max_depth = 0) ?(n = 0) (c1 : S.Key.t) (c2 : S.Key.t) = ()
  > let lca_aux t ?max_depth ?n c1 c2 =
  >   lcas t ?max_depth ?n c1 c2
  > EOF

  $ LOC=4:21
  $ show_location opt_arg.ml $LOC
  module S = struct module Key = struct type t end end
  let lcas t ?(max_depth = 0) ?(n = 0) (c1 : S.Key.t) (c2 : S.Key.t) = ()
  let lca_aux t ?max_depth ?n c1 c2 =
    lcas t ?max_depth ?█ c1 c2

  $ $MERLIN single complete-prefix -prefix n -position $LOC -filename opt_arg.ml <opt_arg.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "n",
          "kind": "Value",
          "desc": "'a option",
          "info": "",
          "deprecated": false
        },
        {
          "name": "nan",
          "kind": "Value",
          "desc": "float",
          "info": "",
          "deprecated": false
        },
        {
          "name": "neg_infinity",
          "kind": "Value",
          "desc": "float",
          "info": "",
          "deprecated": false
        },
        {
          "name": "not",
          "kind": "Value",
          "desc": "bool -> bool",
          "info": "",
          "deprecated": false
        },
        {
          "name": "nativeint",
          "kind": "Type",
          "desc": "type nativeint = external \"nativeint\"",
          "info": "",
          "deprecated": false
        }
      ],
      "context": [
        "application",
        {
          "argument_type": "S.Key.t",
          "labels": [
            {
              "name": "?n",
              "type": "int"
            }
          ]
        }
      ]
    },
    "notifications": []
  }

