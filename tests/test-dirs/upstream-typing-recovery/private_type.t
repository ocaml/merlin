  $ cat >private_type.ml <<'EOF'
  > module T : sig
  >   type t = private int
  >   type r = private Foo of int
  >   val mk : int -> t
  > end = struct
  >   type t = int
  >   type r = Foo of int
  >   let mk x = x
  > end
  > 
  > type k = private A
  > 
  > let r = A
  > 
  > type t = {a : T.t; b : int}
  > 
  > let a = T.mk 12
  > let b = (10 : T.t)
  > 
  > 
  > type t' = private <foo : int>
  > 
  > let f =
  >   let a = T.Foo 10 in
  >   a, 10
  > 
  > let x = {a = 10; b = (a :> int) + 15}
  > EOF

  $ $MERLIN single errors -filename private_type.ml < private_type.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 13,
          "col": 8
        },
        "end": {
          "line": 13,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot create values of the private type k"
      },
      {
        "start": {
          "line": 18,
          "col": 9
        },
        "end": {
          "line": 18,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type T.t"
      },
      {
        "start": {
          "line": 24,
          "col": 10
        },
        "end": {
          "line": 24,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Cannot create values of the private type T.r"
      },
      {
        "start": {
          "line": 27,
          "col": 13
        },
        "end": {
          "line": 27,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type T.t"
      }
    ],
    "notifications": []
  }
