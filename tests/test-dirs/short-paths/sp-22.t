  $ mkdir base
  $ cd base

  $ cat >base.mli <<'EOF'
  > module Error : sig 
  >   type t 
  >   val return : unit -> t
  > end
  > EOF

  $ $OCAMLC -c base.mli

  $ cd ..

  $ cat >core__Error.mli <<'EOF'
  > include module type of struct
  >   include Base.Error
  > end
  > EOF

  $ $OCAMLC -c -I base core__Error.mli
  $ ls
  base
  core__Error.cmi
  core__Error.mli

  $ cat >core.ml <<'EOF'
  > module Error = Core__Error
  > EOF

  $ $OCAMLC -c core.ml

  $ ls
  base
  core.cmi
  core.cmo
  core.ml
  core__Error.cmi
  core__Error.mli


  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > EOF

  $ cat >main.ml <<'EOF'
  > open Core
  > let foo = Core.Error.return ()
  > EOF

We expect Error.t 
  $ $MERLIN single type-enclosing -position 2:5 \
  > -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 7
        },
        "type": "Error.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
