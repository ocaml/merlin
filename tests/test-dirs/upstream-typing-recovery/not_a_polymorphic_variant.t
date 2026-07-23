  $ cat >not_a_polymorphic_variant.ml <<'EOF'
  > type t = int
  > 
  > let f = function #t -> assert false
  > let e x =
  >   let r = match x with
  >     | #t -> 10
  >     | _ -> 11
  >   in
  >   r + 10
  > 
  > module T = struct
  >   let e x =
  >     let r = match x with
  >       | #t -> 10
  >       | _ -> 11
  >     in
  >     r + 10
  > end
  > 
  > let u = (e 10) + (T.e 10) + 7
  > 
  > let t : string = u
  > EOF

  $ $MERLIN single errors -filename not_a_polymorphic_variant.ml < not_a_polymorphic_variant.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 18
        },
        "end": {
          "line": 3,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type t is not a variant type"
      },
      {
        "start": {
          "line": 6,
          "col": 7
        },
        "end": {
          "line": 6,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type t is not a variant type"
      },
      {
        "start": {
          "line": 14,
          "col": 9
        },
        "end": {
          "line": 14,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The type t is not a variant type"
      },
      {
        "start": {
          "line": 22,
          "col": 17
        },
        "end": {
          "line": 22,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value u has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
