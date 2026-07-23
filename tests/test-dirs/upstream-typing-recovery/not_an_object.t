  $ cat >not_an_object.ml <<'EOF'
  > let f (x : int) obj =
  >   let r = x # foo in
  >   let o = obj # bar x in
  >   (o, r)
  > 
  > let t = 10
  > 
  > let g (x : int) obj =
  >   let r = x # foo
  >   and o = obj # bar x in
  >   (r, o)
  > 
  > let r : string = 10
  > EOF

  $ $MERLIN single errors -filename not_an_object.ml < not_an_object.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 10
        },
        "end": {
          "line": 2,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression is not an object; it has type int"
      },
      {
        "start": {
          "line": 9,
          "col": 10
        },
        "end": {
          "line": 9,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression is not an object; it has type int"
      },
      {
        "start": {
          "line": 13,
          "col": 17
        },
        "end": {
          "line": 13,
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
