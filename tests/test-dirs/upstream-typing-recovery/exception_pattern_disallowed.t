  $ cat >exception_pattern_disallowed.ml <<'EOF'
  > let f x =
  >   match x with
  >   | Some (exception Not_found) -> 10
  >   | _ -> 11
  > 
  > let g = function
  >   | exception Not_found -> 1
  >   | x -> 10
  > 
  > let t = (f None) + (g None)
  > 
  > let u : string = t + t
  > EOF

  $ $MERLIN single errors -filename exception_pattern_disallowed.ml < exception_pattern_disallowed.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 9
        },
        "end": {
          "line": 3,
          "col": 30
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Exception patterns are not allowed in this position."
      },
      {
        "start": {
          "line": 7,
          "col": 4
        },
        "end": {
          "line": 7,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Exception patterns are not allowed in this position."
      },
      {
        "start": {
          "line": 12,
          "col": 17
        },
        "end": {
          "line": 12,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
