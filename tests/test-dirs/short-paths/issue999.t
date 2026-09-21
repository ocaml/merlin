
(* *** #999 *** *)

  $ cat >test.ml <<'EOF'
  > module type S = sig
  >   type t
  > 
  >   val foo : int -> t
  > end
  > 
  > module Functor (S: S) : sig
  >   val bar : int -> S.t
  > end = struct
  >   let bar i =
  >     S.foo i
  > end
  > 
  > module Bar = Functor (struct
  >     type t = int
  > 
  >     let foo _i = "haha"
  >   end)
  > EOF

  $ $MERLIN single errors -short-paths \
  > -log-file log -log-section short-paths \
  > -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 14,
          "col": 13
        },
        "end": {
          "line": 18,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Modules do not match: sig type t = int val foo : 'a -> string end
  is not included in S
  Values do not match:
  val foo : 'a -> string
  is not included in
  val foo : int -> int
  The type int -> string is not compatible with the type int -> int
  Type string is not compatible with type int
  File \"test.ml\", line 4, characters 2-20: Expected declaration
  File \"test.ml\", line 17, characters 8-11: Actual declaration"
      }
    ],
    "notifications": []
  }
