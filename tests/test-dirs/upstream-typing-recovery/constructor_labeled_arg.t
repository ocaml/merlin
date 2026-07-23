  $ cat >constructor_labeled_arg.ml <<'EOF'
  > type ('a, 'b) pair = Pair of 'a * 'b
  > let x = Pair (~x: 5, 2)
  > 
  > let f = function
  > | Pair (~x:5, 2) -> true
  > | _ -> false
  > 
  > let r =
  >   let a = f x in
  >   (not a) && false
  > 
  > let t : int = r
  > EOF

  $ $MERLIN single errors -filename constructor_labeled_arg.ml < constructor_labeled_arg.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Constructors cannot have labeled arguments.
  Consider using an inline record instead."
      },
      {
        "start": {
          "line": 5,
          "col": 2
        },
        "end": {
          "line": 5,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Constructors cannot have labeled arguments.
  Consider using an inline record instead."
      },
      {
        "start": {
          "line": 12,
          "col": 14
        },
        "end": {
          "line": 12,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value r has type bool but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
