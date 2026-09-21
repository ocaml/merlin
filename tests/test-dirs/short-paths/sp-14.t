This reproduces issue #14
The difficulty lied in that type equality:
type 'a t = 'a Or_error.t Deferred.t

Normalization will stop here because 'a <> 'a Or_error.t

  $ mkdir async_kernel
  $ cd async_kernel


  $ cat >async_kernel__.ml <<'EOF'
  > module Or_error = Async_kernel__Or_error
  > module Deferred = Async_kernel__Deferred
  > module Deferred0 = Async_kernel__Deferred0
  > module Deferred1 = Async_kernel__Deferred1
  > module Deferred_or_error = Async_kernel__Deferred_or_error
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null

  $ cat >or_error.mli <<'EOF'
  > type nonrec 'a t = ('a, int) Result.t
  > EOF

  $ cat >or_error.ml <<'EOF'
  > type nonrec 'a t = ('a, int) Result.t
  > EOF

  $ $OCAMLC -c or_error.mli -open Async_kernel__ -o Async_kernel__Or_error
  $ $OCAMLC -c or_error.ml -open Async_kernel__ -o Async_kernel__Or_error

  $ cat >deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let return : 'a -> 'a t = Fun.id
  > EOF

  $ cat >deferred0.mli <<'EOF'
  > type +'a t 
  > val return : 'a -> 'a t
  > EOF

  $ $OCAMLC -c deferred0.mli -open Async_kernel__ -o Async_kernel__Deferred0
  $ $OCAMLC -c deferred0.ml -open Async_kernel__ -o Async_kernel__Deferred0

  $ cat >deferred1.ml <<'EOF'
  > include Deferred0
  > let return = Deferred0.return
  > EOF

  $ $OCAMLC -c deferred1.ml -open Async_kernel__ -o Async_kernel__Deferred1

  $ cat >deferred_or_error.mli <<'EOF'
  > module Deferred = Deferred1
  > 
  > type 'a t = 'a Or_error.t Deferred.t
  > val return : 'a -> 'a t 
  > EOF

  $ cat >deferred_or_error.ml <<'EOF'
  > module Deferred = Deferred1
  > 
  > type 'a t = 'a Or_error.t Deferred.t
  > let return x = Deferred.return (Ok x)
  > EOF

  $ $OCAMLC -c deferred_or_error.mli -open Async_kernel__ -o Async_kernel__Deferred_or_error
  $ $OCAMLC -c deferred_or_error.ml -open Async_kernel__ -o Async_kernel__Deferred_or_error
  $ ocamlobjinfo -quiet -discourse Async_kernel__Deferred_or_error.cmi
  Discourse:
  Deferred: alias: Deferred1 [Async_kernel__!.Deferred1]
    Async_kernel__!.Deferred1
  
  t: Deferred/312.t; Async_kernel__!.Or_error.t
  return: t/313


  $ cat >deferred.ml <<'EOF'
  > include Deferred1
  > module Or_error = Deferred_or_error
  > EOF

  $ cat >deferred.mli <<'EOF'
  > type +'a t = 'a Deferred1.t
  > val return : 'a -> 'a t
  > module Or_error = Deferred_or_error
  > EOF

  $ $OCAMLC -c deferred.mli -open Async_kernel__ -o Async_kernel__Deferred
  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred
  $ ocamlobjinfo -quiet -discourse Async_kernel__Deferred.cmi
  Discourse:
  t: Async_kernel__!.Deferred1.t
  return: t/286
  Or_error: alias: Deferred_or_error [Async_kernel__!.Deferred_or_error]
    Async_kernel__!.Deferred_or_error
  

  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > EOF

  $ $OCAMLC -c async_kernel.ml -open Async_kernel__
  $ ocamlobjinfo -quiet -discourse async_kernel.cmi
  Discourse:
  Deferred: alias: Deferred [Async_kernel__!.Deferred] Async_kernel__!.Deferred
  

  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > open! Async_kernel
  > include Async_kernel
  > 
  > module Deferred = struct
  >   include Deferred
  > 
  >   module Or_error = struct
  >     include Async_kernel.Deferred.Or_error
  >   end
  > end
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel
  $ ocamlobjinfo -quiet -discourse async.cmi
  Discourse:
  Deferred: 
  Deferred.t: Async_kernel__!.Deferred1.t
  Deferred.return: t/342
  Deferred.Or_error: 
  Deferred.Or_error.Deferred: alias: Deferred1 [Async_kernel__!.Deferred1]
    Async_kernel__!.Deferred1
  
  Deferred.Or_error.t: Deferred/345.t; Async_kernel__!.Or_error.t
  Deferred.Or_error.return: t/346

  $ cd ..

  $ cat >test.ml <<'EOF'
  > open! Async
  > let () = ignore Deferred.Or_error.return
  > let foo = Async_kernel.Deferred.Or_error.return 5
  > EOF


  $ $OCAMLC -c test.ml  -I async -I async_kernel

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

We expect int Deferred.Or_error.t
  $ $MERLIN single type-enclosing -position 3:5 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "int Deferred.Or_error.t"
