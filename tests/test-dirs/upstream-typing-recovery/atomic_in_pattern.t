  $ cat >atomic_in_pattern.ml <<'EOF'
  > module E = struct
  >   type t = { x : int; mutable y : int [@atomic] }
  > 
  >   let forbidden { x; y } = x + y
  > end
  > 
  > let k = E.forbidden E.{x = 10; y = 11} + 12
  > let r : string = k
  > EOF

  $ $MERLIN single errors -filename atomic_in_pattern.ml < atomic_in_pattern.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Atomic fields (here y) are forbidden in patterns,
  as it is difficult to reason about when the atomic read
  will happen during pattern matching: the field may be read
  zero, one or several times depending on the patterns around it."
      },
      {
        "start": {
          "line": 8,
          "col": 17
        },
        "end": {
          "line": 8,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value k has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
