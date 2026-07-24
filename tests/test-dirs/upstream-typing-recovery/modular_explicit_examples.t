FIXME RECOVERY Divergence from the compiler reference: Merlin aborts with a fatal error ("The
module F is not a functor, it cannot be applied.") and recovers nothing (0
diagnostics), whereas the compiler recovers 19 errors. (see claude-report.md)

  $ cat >modular_explicit_examples.ml <<'EOF'
  > module type Typ = sig type t end
  > 
  > module type Add = sig type t val add : t -> t -> t end
  > 
  > 
  > module type Add2 = sig
  >   type a
  >   type t
  >   val add : t -> t -> t
  > end
  > 
  > module type Add3 = sig
  >   type t
  >   type a
  >   val add : t -> t -> t
  > end
  > 
  > module type Add4 = sig
  >   type t
  >   val add : t -> t -> t
  >   type a
  > end
  > 
  > module type TypPrivInt = sig
  >   type t = private int
  > end
  > 
  > let id (module T : Typ) (x : T.t) = x
  > 
  > let id2 : (module T : Typ) -> T.t -> T.t =
  >   fun (module A : Typ) (x : A.t) -> x
  > 
  > let id_infer_sig  : (module T : Typ) -> T.t -> T.t =
  >   fun (module A) (x : A.t) -> x
  > 
  > let f x (module M : Typ) (y : M.t) = (x, y)
  > 
  > (* Here we cannot extract the type of m *)
  > let invalid_arg3 =
  >   let m = (module Int : Typ) in
  >   f 3 m 4
  > 
  > (* Here we cannot extract the type of m. This could be accepted because m does
  >    not hide any abstract types. *)
  > let invalid_arg4 =
  >   let m = (module Int : Typ with type t = int) in
  >   f 3 m 4
  > 
  > let foo f a =
  >   let _ = (f ~a : (module M : Typ) -> M.t) in
  >   f ~a (fun x -> x)
  > 
  > let c : string = 10
  > 
  > let labelled () (module M : Typ) ~(y:M.t) = y
  > 
  > let apply_labelled = labelled () ~y:3 (module Int)
  > 
  > let apply_labelled_fail = labelled () ~y:3
  > 
  > let foo2 f a =
  >   let m = (module Int : Typ) in
  >   let _ = (f ~a : (module M : Typ) -> M.t) in
  >   f ~a m
  > 
  > let apply_labelled_success = labelled' ~y:3
  > 
  > let foo4 f a =
  >   let _ = (f ~a : b:(module M : Typ) -> M.t) in
  >   f ~a ~c:(module Int)
  > 
  > let x_from_struct = id (module struct type t = int end) 3
  > 
  > let x_from_generative_functor = id (module F ())
  > 
  > module type Map = sig
  >   type _ t
  >   val map : ('a -> 'b) -> 'a t -> 'b t
  > end
  > 
  > let map (module M : Map) f x = M.map f x
  > 
  > module MapCombine (M1 : Map) (M2 : Map) = struct
  >   type 'a t = 'a M1.t M2.t
  >   let map f = map (module M2) (map (module M1) f)
  > end
  > 
  > let s_list_array = map (module MapCombine(List)(Array))
  >     string_of_int [|[3; 2]; [2]; []|]
  > 
  > let s_list_arrayb =
  >     map
  >       (module MapCombine(struct
  >           type 'a t = A of 'a
  >           let map f (A x) = (A (f x))
  >         end)(Array))
  >       string_of_int [|[]|]
  > 
  > let fail = map (module F()) string_of_int [3]
  > 
  > 
  > (* But we can add type fields with ground coercion *)
  > let coerce5 (f : (module A : Add) -> A.t -> A.t) =
  >   (f :> (module A : Add2) -> A.t -> A.t)
  > 
  > (* changing type order in signature *)
  > let try_coerce6 (f : (module A : Add2) -> A.t -> A.t) =
  >   (f : (module A : Add3) -> A.t -> A.t)
  > 
  > let try_coerce7 (f : (module A : Add2) -> A.t -> A.t) =
  >   (f : (module A : Add4) -> A.t -> A.t)
  > 
  > let try_coerce8 (f : (module A : Add2) -> A.t -> A.t) =
  >   (f :> (module A : Add) -> A.t -> A.t)
  > 
  > let restrict_signature_with_priv_to_remove_dep =
  >   fun (x : (module T : Typ) -> unit -> T.t) ->
  >     (x :> (module TypPrivInt) -> unit -> int)
  > 
  > let restrict_signature_with_priv_to_add_dep =
  >   fun (x : (module Typ) -> int -> int) ->
  >   (x :> (module T : TypPrivInt) -> T.t -> int)
  > 
  > let last_error : string = 10
  > let r = last_error ^ "foo"
  > 
  > 
  > type t = (module X:Typ) -> X.t -> X.t
  > let f: t= fun (module F_name) x -> x
  > 
  > let err (f:t) (g: (module Name_A:Typ) -> Name_A.t -> int as 'a) =
  >   (g: float),
  >   (f:'a)
  > EOF

  $ $MERLIN single errors -filename modular_explicit_examples.ml < modular_explicit_examples.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 41,
          "col": 2
        },
        "end": {
          "line": 41,
          "col": 5
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 41,
              "col": 2
            },
            "end": {
              "line": 41,
              "col": 5
            },
            "message": "This function is module-dependent. The dependency is preserved
  when the function is passed a static module argument (module M : S)
  or (module M). Its argument here is not static, so the type-checker
  tried instead to change the function type to be non-dependent."
          }
        ],
        "valid": true,
        "message": "This expression has type (module M : Typ) -> M.t -> 'a * M.t
  but an expression was expected of type (module Typ) -> 'b
  The module M would escape its scope"
      },
      {
        "start": {
          "line": 47,
          "col": 2
        },
        "end": {
          "line": 47,
          "col": 5
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 47,
              "col": 2
            },
            "end": {
              "line": 47,
              "col": 5
            },
            "message": "This function is module-dependent. The dependency is preserved
  when the function is passed a static module argument (module M : S)
  or (module M). Its argument here is not static, so the type-checker
  tried instead to change the function type to be non-dependent."
          }
        ],
        "valid": true,
        "message": "This expression has type (module M : Typ) -> M.t -> 'a * M.t
  but an expression was expected of type (module Typ) -> 'b
  The module M would escape its scope"
      },
      {
        "start": {
          "line": 47,
          "col": 6
        },
        "end": {
          "line": 47,
          "col": 7
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value m has type (module Typ with type t = int)
  but an expression was expected of type (module Typ)
  The constraint on t in the first module type is not compatible
  with the declaration of type t in the second module type."
      },
      {
        "start": {
          "line": 51,
          "col": 2
        },
        "end": {
          "line": 51,
          "col": 6
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 51,
              "col": 2
            },
            "end": {
              "line": 51,
              "col": 6
            },
            "message": "This function is module-dependent. The dependency is preserved
  when the function is passed a static module argument (module M : S)
  or (module M). Its argument here is not static, so the type-checker
  tried instead to change the function type to be non-dependent."
          }
        ],
        "valid": true,
        "message": "This expression has type (module M : Typ) -> M.t
  but an expression was expected of type (module Typ) -> 'a
  The module M would escape its scope"
      },
      {
        "start": {
          "line": 51,
          "col": 7
        },
        "end": {
          "line": 51,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a function, the expected type is (module Typ)"
      },
      {
        "start": {
          "line": 53,
          "col": 17
        },
        "end": {
          "line": 53,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 59,
          "col": 26
        },
        "end": {
          "line": 59,
          "col": 37
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This function has type (module M : Typ) -> y:M.t -> M.t
  The module argument M cannot be omitted in this application."
      },
      {
        "start": {
          "line": 64,
          "col": 2
        },
        "end": {
          "line": 64,
          "col": 6
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 64,
              "col": 2
            },
            "end": {
              "line": 64,
              "col": 6
            },
            "message": "This function is module-dependent. The dependency is preserved
  when the function is passed a static module argument (module M : S)
  or (module M). Its argument here is not static, so the type-checker
  tried instead to change the function type to be non-dependent."
          }
        ],
        "valid": true,
        "message": "This expression has type (module M : Typ) -> M.t
  but an expression was expected of type (module Typ) -> 'a
  The module M would escape its scope"
      },
      {
        "start": {
          "line": 66,
          "col": 29
        },
        "end": {
          "line": 66,
          "col": 38
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 66,
              "col": 29
            },
            "end": {
              "line": 66,
              "col": 38
            },
            "message": "Hint: Did you mean labelled?"
          }
        ],
        "valid": true,
        "message": "Unbound value labelled'"
      },
      {
        "start": {
          "line": 70,
          "col": 10
        },
        "end": {
          "line": 70,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The function applied to this argument has type b:(module M : Typ) -> M.t
  This argument cannot be applied with label ~c"
      },
      {
        "start": {
          "line": 74,
          "col": 43
        },
        "end": {
          "line": 74,
          "col": 44
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module F"
      },
      {
        "start": {
          "line": 92,
          "col": 4
        },
        "end": {
          "line": 92,
          "col": 7
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 92,
              "col": 4
            },
            "end": {
              "line": 92,
              "col": 7
            },
            "message": "This function is module-dependent. The dependency is preserved
  when the function is passed a static module argument (module M : S)
  or (module M). Its argument here is not static, so the type-checker
  tried instead to change the function type to be non-dependent."
          }
        ],
        "valid": true,
        "message": "This expression has type (module M : Map) -> ('a -> 'b) -> 'a M.t -> 'b M.t
  but an expression was expected of type (module Map) -> 'c
  The module M would escape its scope"
      },
      {
        "start": {
          "line": 93,
          "col": 14
        },
        "end": {
          "line": 96,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This functor has type
  (M1 : Map) (M2 : Map) ->
    sig
      type 'a t = 'a M1.t M2.t
      val map : ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t
    end
  The parameter cannot be eliminated in the result type.
  Please bind the argument to a module identifier."
      },
      {
        "start": {
          "line": 93,
          "col": 14
        },
        "end": {
          "line": 96,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Signature mismatch:
  Modules do not match: sig end is not included in Map
  The type t is required but not provided
  File \"modular_explicit_examples.ml\", line 77, characters 2-10:
    Expected declaration
  The value map is required but not provided
  File \"modular_explicit_examples.ml\", line 78, characters 2-38:
    Expected declaration"
      },
      {
        "start": {
          "line": 99,
          "col": 23
        },
        "end": {
          "line": 99,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module F"
      },
      {
        "start": {
          "line": 114,
          "col": 2
        },
        "end": {
          "line": 114,
          "col": 39
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Type (module A : Add2) -> A.t -> A.t is not a subtype of
    (module A : Add) -> A.t -> A.t
  Modules do not match: Add is not included in Add2
  The type a is required but not provided
  File \"modular_explicit_examples.ml\", line 7, characters 2-8:
    Expected declaration"
      },
      {
        "start": {
          "line": 124,
          "col": 26
        },
        "end": {
          "line": 124,
          "col": 28
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 132,
          "col": 3
        },
        "end": {
          "line": 132,
          "col": 4
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value g has type (module Name_A : Typ) -> Name_A.t -> int
  but an expression was expected of type float"
      },
      {
        "start": {
          "line": 133,
          "col": 3
        },
        "end": {
          "line": 133,
          "col": 4
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value f has type (module X : Typ) -> X.t -> X.t
  but an expression was expected of type (module X : Typ) -> X.t -> int
  Type X.t is not compatible with type int"
      }
    ],
    "notifications": []
  }
