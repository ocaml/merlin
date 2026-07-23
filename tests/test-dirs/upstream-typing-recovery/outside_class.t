  $ cat >outside_class.ml <<'EOF'
  > let x = {< y = 12 >}
  > 
  > let t =
  >   let o = {< foo = 10 >} in
  >   let b = object
  >     val x = 10
  >       val y = {<x = 15>}
  >     method foo = {<x = 11>}
  >   end in o, b
  > 
  > let x : string = 10
  > EOF

  $ $MERLIN single errors -filename outside_class.ml < outside_class.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This object duplication occurs outside a method definition"
      },
      {
        "start": {
          "line": 4,
          "col": 10
        },
        "end": {
          "line": 4,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This object duplication occurs outside a method definition"
      },
      {
        "start": {
          "line": 7,
          "col": 14
        },
        "end": {
          "line": 7,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This object duplication occurs outside a method definition"
      },
      {
        "start": {
          "line": 11,
          "col": 17
        },
        "end": {
          "line": 11,
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
