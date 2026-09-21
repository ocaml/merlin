  $ cat >async_kernel__Deferred.mli <<'EOF'
  > type t
  > val return : unit -> t
  > EOF

  $ $OCAMLC -c async_kernel__Deferred.mli
  $ ls
  async_kernel__Deferred.cmi
  async_kernel__Deferred.mli

  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > EOF

  $ cat >async.ml <<'EOF'
  > open! Async_kernel
  > include Async_kernel
  > module Deferred = struct
  >   include Deferred
  > end
  > EOF

  $ $OCAMLC -c async_kernel.ml async.ml

  $ ls
  async.cmi
  async.cmo
  async.ml
  async_kernel.cmi
  async_kernel.cmo
  async_kernel.ml
  async_kernel__Deferred.cmi
  async_kernel__Deferred.mli


  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > EOF

  $ cat >main.ml <<'EOF'
  > open Async
  > let foo = Async_kernel.Deferred.return ()
  > EOF

We expect Deferred.t 
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
        "type": "Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
