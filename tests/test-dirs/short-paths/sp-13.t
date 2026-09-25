This mocks the Async --include--> Async_kernel --exports--> Deferred

  $ mkdir async_kernel
  $ cd async_kernel


  $ cat >async_kernel__.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > module Deferred0 = Async_kernel__Deferred0
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null
  $ ocamlobjinfo -quiet -discourse async_kernel__.cmi
  Discourse:
  Deferred: alias: Async_kernel__Deferred [Async_kernel__Deferred!]
    Async_kernel__Deferred!
  
  Deferred0: alias: Async_kernel__Deferred0 [Async_kernel__Deferred0!]
    Async_kernel__Deferred0!
  
  $ cat >deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let create : 'a -> 'a t = fun x -> x
  > EOF

  $ cat >deferred0.mli <<'EOF'
  > type +'a t 
  > val create : 'a -> 'a t
  > EOF

  $ $OCAMLC -c deferred0.mli -open Async_kernel__ -o Async_kernel__Deferred0
  $ $OCAMLC -c deferred0.ml -open Async_kernel__ -o Async_kernel__Deferred0



  $ cat >deferred.ml <<'EOF'
  > type +'a t = 'a Deferred0.t
  > 
  > module Let_syntax = struct 
  >   module Let_syntax = struct let return x = Deferred0.create x end
  > end
  > EOF

  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred

  $ ocamlobjinfo -quiet -discourse Async_kernel__Deferred.cmi
  Discourse:
  t: Async_kernel__!.Deferred0.t
  Let_syntax: 
  Let_syntax.Let_syntax: 
  Let_syntax.Let_syntax.return: 

  $ ocamlobjinfo -quiet -discourse Async_kernel__.cmi
  Discourse:
  Deferred: alias: Async_kernel__Deferred [Async_kernel__Deferred!]
    Async_kernel__Deferred!
  
  Deferred0: alias: Async_kernel__Deferred0 [Async_kernel__Deferred0!]
    Async_kernel__Deferred0!
  


  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > include Deferred.Let_syntax
  > EOF

  $ $OCAMLC -c async_kernel.ml -open Async_kernel__

  $ ocamlobjinfo -quiet -discourse async_kernel.cmi
  Discourse:
  Deferred: alias: Deferred [Async_kernel__!.Deferred] Async_kernel__!.Deferred
  
  Let_syntax:
    alias: Deferred.Let_syntax.Let_syntax [Deferred/283.Let_syntax.Let_syntax] 
  

  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > include Async_kernel
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel
  $ ocamlobjinfo -quiet -discourse async.cmi
  Discourse:
  Deferred: alias: Deferred [Async_kernel__!.Deferred] Async_kernel__!.Deferred
  
  Let_syntax:
    alias: Deferred.Let_syntax.Let_syntax [Deferred/278.Let_syntax.Let_syntax] 
  
  $ cd ..

  $ cat >test.ml <<'EOF'
  > open! Async
  > 
  > let foo = Let_syntax.return 5
  > EOF


  $ $OCAMLC -c test.ml  -I async -I async_kernel

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

  $ $MERLIN single type-enclosing -position 3:5 \
  > -filename test.ml < test.ml | jq '.value[].type'
  "int Deferred.t"

Dump the discourse so regressions show up as a diff in this test:

  $ $MERLIN single type-enclosing -nostdlib -position 3:5 \
  > -log-file - -log-section discourse-recap \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [Async! -> {item = (module, Async!)};
    Async!.Let_syntax -> {item = (module, Async!.Let_syntax)};
    Async!.Let_syntax.return -> {item = (value, Async!.Let_syntax.return)}];
    substs =
    Async!.Deferred -> [Deferred/17];
    Async!.Let_syntax -> [Let_syntax/18];
    Async_kernel__!.Deferred -> [Deferred/17];
    Async_kernel__Deferred!.Let_syntax.Let_syntax -> [Let_syntax/18] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 74;
    paths =
    Let_syntax/18 [module: Async!.Let_syntax];
    Deferred/17 [module: Async!.Deferred];
    Async_kernel__Deferred! [module: Async_kernel__Deferred!];
    Async_kernel__Deferred!.t [type: Async_kernel__Deferred!.t];
    Async_kernel__Deferred!.Let_syntax
      [module: Async_kernel__Deferred!.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax
      [module: Async_kernel__Deferred!.Let_syntax.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax.return
      [value: Async_kernel__Deferred!.Let_syntax.Let_syntax.return];
    Async_kernel__! [module: Async_kernel__!];
    Async_kernel__!.Deferred0 [module: Async_kernel__!.Deferred0];
    Async_kernel__!.Deferred [module: Async_kernel__!.Deferred];
    Async! [module: Async!];
    Async!.Let_syntax [module: Async!.Let_syntax];
    Async!.Let_syntax.return [value: Async!.Let_syntax.return];
    Async!.Deferred [module: Async!.Deferred];
    Async!.Deferred.Let_syntax [module: Async!.Deferred.Let_syntax];
    Async!.Deferred.Let_syntax.Let_syntax
      [module: Async!.Deferred.Let_syntax.Let_syntax];
    atomic_loc/21! [type: atomic_loc/21!];
    iarray/20! [type: iarray/20!];
    floatarray/19! [type: floatarray/19!];
    extension_constructor/18! [type: extension_constructor/18!];
    string/17! [type: string/17!];
    lazy_t/16! [type: lazy_t/16!];
    int64/15! [type: int64/15!];
    int32/14! [type: int32/14!];
    nativeint/13! [type: nativeint/13!];
    option/12! [type: option/12!];
    list/11! [type: list/11!];
    array/10! [type: array/10!];
    continuation/9! [type: continuation/9!];
    eff/8! [type: eff/8!];
    exn/7! [type: exn/7!];
    unit/6! [type: unit/6!];
    bool/5! [type: bool/5!];
    float/4! [type: float/4!];
    bytes/3! [type: bytes/3!];
    char/2! [type: char/2!];
    int/1! [type: int/1!];
    substs =
    [Async_kernel__Deferred! -> [Async!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Async!.Deferred -> [Deferred/17; Async_kernel__!.Deferred];
    Async!.Let_syntax -> [Let_syntax/18; Async!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Deferred/17];
    Async_kernel__Deferred!.Let_syntax -> [Async!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Let_syntax/18; Async!.Let_syntax; Async!.Deferred.Let_syntax.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    Async_kernel__Deferred! -> [Async!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Async!.Deferred -> [Deferred/17; Async_kernel__!.Deferred];
    Async!.Let_syntax -> [Let_syntax/18; Async!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Deferred/17];
    Async_kernel__Deferred!.Let_syntax -> [Async!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Let_syntax/18; Async!.Let_syntax; Async!.Deferred.Let_syntax.Let_syntax] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred! -> [Async!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Async!.Deferred -> [Deferred/17; Async_kernel__!.Deferred];
    Async!.Let_syntax -> [Let_syntax/18; Async!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Deferred/17];
    Async_kernel__Deferred!.Let_syntax -> [Async!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Let_syntax/18; Async!.Let_syntax; Async!.Deferred.Let_syntax.Let_syntax]]
    }
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
