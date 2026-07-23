  $ cat >abstract_wrong_label.ml <<'EOF'
  > let (f : x:int -> int) = fun y -> y
  > 
  > let t =
  >   let (f : int -> int) = fun ~y -> y in
  >   f 10
  > 
  > module R = struct
  > class t =
  >   object
  >     method foo (f : int -> int) = f ~x:10
  >   end
  > end
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename abstract_wrong_label.ml < abstract_wrong_label.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 25
        },
        "end": {
          "line": 1,
          "col": 35
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This function should have type x:int -> int
  but its first argument is unlabeled instead of being labeled ~x"
      },
      {
        "start": {
          "line": 4,
          "col": 25
        },
        "end": {
          "line": 4,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This function should have type int -> int
  but its first argument is labeled ~y instead of being unlabeled"
      },
      {
        "start": {
          "line": 10,
          "col": 39
        },
        "end": {
          "line": 10,
          "col": 41
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The function applied to this argument has type int -> int
  This argument cannot be applied with label ~x"
      },
      {
        "start": {
          "line": 14,
          "col": 17
        },
        "end": {
          "line": 14,
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
