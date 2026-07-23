  $ cat >bindings_type_clash.ml <<'EOF'
  > let apply x f = f x
  > let pair x y = x, y
  > 
  > module A = struct
  > 
  >   let (let+) = (fun x f -> not x)
  >   let (and+) = pair
  > 
  >   let f =
  >     let+ x = 1
  >     and+ y = 2
  >     and+ z = 3 in
  >     x + y + z
  > end
  > 
  > module B = struct
  > 
  >   let (let+) = apply
  >   let (and+) = fun x y -> x + 1, y
  > 
  >   let f =
  >     let+ x = 1
  >     and+ y = 2
  >     and+ z = 3 in
  >     x + y + z
  > end
  > EOF

  $ $MERLIN single errors -filename bindings_type_clash.ml < bindings_type_clash.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 10,
          "col": 9
        },
        "end": {
          "line": 12,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "These bindings have type (int * int) * int but bindings were expected of type
    bool"
      },
      {
        "start": {
          "line": 22,
          "col": 9
        },
        "end": {
          "line": 23,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "These bindings have type int * int but bindings were expected of type int"
      }
    ],
    "notifications": []
  }
