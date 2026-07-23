  $ cat >missing_type_constraint.ml <<'EOF'
  > type _ ty = Int : int ty
  > type dyn = Dyn : 'a ty * 'a -> dyn
  > 
  > 
  > let a = function Dyn (type a) (w, x) -> 10
  > let b =
  >   let x = function Dyn (type a) (w, x) -> 10 in
  >   let y = function Dyn (type a) (c, k) -> 10 in
  >   x (Dyn (Int, 1)) + y (Dyn (Int, 2))
  > 
  > 
  > let r = (a (Dyn (Int, 42))) + b
  > let t : string = r
  > EOF

  $ $MERLIN single errors -filename missing_type_constraint.ml < missing_type_constraint.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 30
        },
        "end": {
          "line": 5,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types introduced in a constructor pattern
  must be bound by a type constraint on the argument."
      },
      {
        "start": {
          "line": 7,
          "col": 32
        },
        "end": {
          "line": 7,
          "col": 38
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types introduced in a constructor pattern
  must be bound by a type constraint on the argument."
      },
      {
        "start": {
          "line": 8,
          "col": 32
        },
        "end": {
          "line": 8,
          "col": 38
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Existential types introduced in a constructor pattern
  must be bound by a type constraint on the argument."
      },
      {
        "start": {
          "line": 13,
          "col": 17
        },
        "end": {
          "line": 13,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value r has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
