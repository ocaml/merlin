  $ cat >apply_wrong_label.ml <<'EOF'
  > let g ~x = x + 1
  > 
  > let _ = g ~y:10
  > 
  > let h ?x y = match x with None -> y | Some v -> v + y
  > 
  > let _ = h ~y:5 10
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename apply_wrong_label.ml < apply_wrong_label.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 15
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 5: this function application is partial, maybe some arguments are
    missing."
      },
      {
        "start": {
          "line": 3,
          "col": 13
        },
        "end": {
          "line": 3,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The function applied to this argument has type x:int -> int
  This argument cannot be applied with label ~y"
      },
      {
        "start": {
          "line": 7,
          "col": 13
        },
        "end": {
          "line": 7,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The function applied to this argument has type ?x:int -> int
  This argument cannot be applied with label ~y"
      },
      {
        "start": {
          "line": 9,
          "col": 17
        },
        "end": {
          "line": 9,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
