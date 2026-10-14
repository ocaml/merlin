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
  > -log-file - -log-section discourse-verbose \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-verbose - U1
  U1: path module used in file: Async! (File "test.ml", line 1, characters 6-11)
  # discourse-verbose - U3
  U3: components of module Async! brought in scope
  # discourse-verbose - U3
  U3: module Deferred/19 brought in scope by open
  # discourse-verbose - U3
  U3: module Deferred/19 [Async!.Deferred] brought in scope by an open
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Deferred/19 (open/alias defined in file)
  # discourse-verbose - subst
  subst: Async!.Deferred -> Deferred/19 (open/alias defined in file)
  # discourse-verbose - U3
  U3: module Let_syntax/20 brought in scope by open
  # discourse-verbose - U3
  U3: module Let_syntax/20 [Async!.Let_syntax] brought in scope by an open
  # discourse-verbose - subst
  subst: Async_kernel__Deferred!.Let_syntax.Let_syntax -> Let_syntax/20 (open/alias defined in file)
  # discourse-verbose - subst
  subst: Async!.Let_syntax -> Let_syntax/20 (open/alias defined in file)
  # discourse-verbose - U1
  U1: path value used in file: Async!.Let_syntax.return (File "test.ml", line 3, characters 10-27)
  # discourse-verbose - U1
  U1: path module used in file: Async!.Let_syntax (File "test.ml", line 3, characters 10-27)
  # discourse-verbose - U1
  U1: path module used in file: Async! (File "test.ml", line 3, characters 10-27)
  # discourse-verbose - D2
  D2: Async! in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async!
  # discourse-verbose - D3
  D3: signature component module Deferred/17 (Async!.Deferred) under module Async!
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred! -> Async!.Deferred (sub-component is a module alias)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async!.Deferred (open/alias defined in file)
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred! -> Async!.Deferred (sub-component is a module alias)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async!.Deferred (open/alias defined in file)
  # discourse-verbose - D3
  D3: signature component module Let_syntax/18 (Async!.Let_syntax) under module Async!
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred!.Let_syntax.Let_syntax -> Async!.Let_syntax (sub-component is a module alias)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred!.Let_syntax.Let_syntax -> Async!.Let_syntax (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async!.Let_syntax in U so in D (kind: module)
  # discourse-verbose - D12S
  D12S: subst Async!.Let_syntax -> Deferred.Let_syntax.Let_syntax[Async!.Deferred.Let_syntax.Let_syntax] (alias at File "async_kernel.ml", line 2, characters 8-27)
  # discourse-verbose - subst
  subst: Async!.Deferred.Let_syntax.Let_syntax -> Async!.Let_syntax (open/alias defined in file)
  # discourse-verbose - D5
  D5: merging discourse of module Async!.Let_syntax
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred!.Let_syntax.Let_syntax -> Async!.Let_syntax (Mty_alias target)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred!.Let_syntax.Let_syntax -> Async!.Let_syntax (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async!.Let_syntax.return in U so in D (kind: value)
  # discourse-verbose - D4
  D4: merging discourse of value Async!.Let_syntax.return
  # discourse-verbose - D2
  D2: Async_kernel__Deferred!.Let_syntax.Let_syntax in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async_kernel__Deferred!.Let_syntax.Let_syntax
  # discourse-verbose - D3
  D3: signature component value return/10 (Async_kernel__Deferred!.Let_syntax.Let_syntax.return) under module Async_kernel__Deferred!.Let_syntax.Let_syntax
  # discourse-verbose - D2
  D2: Async!.Deferred in U so in D (kind: module)
  # discourse-verbose - D12S
  D12S: subst Async!.Deferred -> Deferred[Async_kernel__!.Deferred] (alias at File "async_kernel.ml", line 1, characters 18-26)
  # discourse-verbose - subst
  subst: Async_kernel__!.Deferred -> Async!.Deferred (open/alias defined in file)
  # discourse-verbose - D5
  D5: merging discourse of module Async!.Deferred
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred! -> Async!.Deferred (Mty_alias target)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async!.Deferred (open/alias defined in file)
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred! -> Async!.Deferred (Mty_alias target)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async!.Deferred (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async!.Deferred.Let_syntax in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async!.Deferred.Let_syntax
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred!.Let_syntax -> Async!.Deferred.Let_syntax (Mty_alias target)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred!.Let_syntax -> Async!.Deferred.Let_syntax (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async!.Deferred.Let_syntax.Let_syntax in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async!.Deferred.Let_syntax.Let_syntax
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred!.Let_syntax.Let_syntax -> Async!.Deferred.Let_syntax.Let_syntax (Mty_alias target)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred!.Let_syntax.Let_syntax -> Async!.Deferred.Let_syntax.Let_syntax (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async_kernel__Deferred! in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async_kernel__Deferred!
  # discourse-verbose - D3
  D3: signature component type t/21 (Async_kernel__Deferred!.t) under module Async_kernel__Deferred!
  # discourse-verbose - D3
  D3: signature component module Let_syntax/22 (Async_kernel__Deferred!.Let_syntax) under module Async_kernel__Deferred!
  # discourse-verbose - D2
  D2: Async_kernel__! in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async_kernel__!
  # discourse-verbose - D3
  D3: signature component module Deferred/24 (Async_kernel__!.Deferred) under module Async_kernel__!
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred! -> Async_kernel__!.Deferred (sub-component is a module alias)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async_kernel__!.Deferred (open/alias defined in file)
  # discourse-verbose - D3
  D3: signature component module Deferred0/25 (Async_kernel__!.Deferred0) under module Async_kernel__!
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred0! -> Async_kernel__!.Deferred0 (sub-component is a module alias)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred0! -> Async_kernel__!.Deferred0 (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async_kernel__!.Deferred in U so in D (kind: module)
  # discourse-verbose - D12S
  D12S: subst Async_kernel__!.Deferred -> Async_kernel__Deferred[Async_kernel__Deferred!] (alias at File "async_kernel__.ml", line 1, characters 18-40)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async_kernel__!.Deferred (open/alias defined in file)
  # discourse-verbose - D5
  D5: merging discourse of module Async_kernel__!.Deferred
  # discourse-verbose - D12
  D12: subst Async_kernel__Deferred! -> Async_kernel__!.Deferred (Mty_alias target)
  # discourse-verbose - subst
  subst: Async_kernel__Deferred! -> Async_kernel__!.Deferred (open/alias defined in file)
  # discourse-verbose - D2
  D2: Async_kernel__Deferred!.Let_syntax in U so in D (kind: module)
  # discourse-verbose - D5
  D5: merging discourse of module Async_kernel__Deferred!.Let_syntax
  # discourse-verbose - D3
  D3: signature component module Let_syntax/23 (Async_kernel__Deferred!.Let_syntax.Let_syntax) under module Async_kernel__Deferred!.Let_syntax
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
