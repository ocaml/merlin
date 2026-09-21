This is not a correct reproduction for #14 but did show unexpected behavior at
some point.

  $ mkdir async_kernel
  $ cd async_kernel

  $ cat >async_kernel__.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null

  $ cat >deferred.ml <<'EOF'
  > type +'a t = 'a
  > let create = Fun.id
  > EOF

  $ cat >deferred.mli <<'EOF'
  > type +'a t 
  > val create : 'a -> 'a t
  > EOF

  $ $OCAMLC -c deferred.mli -open Async_kernel__ -o Async_kernel__Deferred
  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred

  $ cat >async_kernel.ml <<'EOF'
  >  module Deferred = Deferred
  > EOF

  $ $OCAMLC -c async_kernel.ml  -open Async_kernel__


  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > open! Async_kernel
  > include Async_kernel
  > 
  > module Deferred = struct
  >   include Deferred
  > end
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel

  $ cd ..

  $ cat >test.ml <<'EOF'
  > open! Async
  > 
  > let foo = Async_kernel.Deferred.create 5
  > EOF

  $ $OCAMLC -c test.ml  -I async -I async_kernel

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

  $ ocamlobjinfo -quiet -discourse async/async.cmi
  Discourse:
  Deferred: 
  Deferred.t: 
  Deferred.create: t/288

  $ ocamlobjinfo -quiet -discourse async_kernel/async_kernel.cmi
  Discourse:
  Deferred: alias: Deferred [Async_kernel__!.Deferred] Async_kernel__!.Deferred
  

  $ ocamlobjinfo -quiet -discourse async_kernel/async_kernel__Deferred.cmi
  Discourse:
  t: 
  create: t/277

  $ $MERLIN single type-enclosing -position 3:5 \
  > -filename test.ml < test.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 7
        },
        "type": "int Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
