  $ cat >not_a_packed_module.ml <<'EOF'
  > let f x =
  >   x + 1
  > 
  > let x =
  >   let a = f (module struct end)
  >   and b = f 10 in
  >   a + b
  > 
  > module U = struct
  >   let x =
  >     let a = f (module struct end)
  >     and b = f 10 in
  >     a + b
  > end
  > 
  > let t : string = x + U.x
  > EOF

  $ $MERLIN single errors -filename not_a_packed_module.ml < not_a_packed_module.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 12
        },
        "end": {
          "line": 5,
          "col": 31
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression is packed module, but the expected type is int"
      },
      {
        "start": {
          "line": 11,
          "col": 14
        },
        "end": {
          "line": 11,
          "col": 33
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression is packed module, but the expected type is int"
      },
      {
        "start": {
          "line": 16,
          "col": 17
        },
        "end": {
          "line": 16,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
