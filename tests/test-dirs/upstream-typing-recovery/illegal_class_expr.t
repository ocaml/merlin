  $ cat >illegal_class_expr.ml <<'EOF'
  > class a =
  >   let _ = new b in
  >   object end
  > and b =
  >   let _ = new a in
  >   object end
  > 
  > class c =
  >   let _ = new c in
  >   object end
  > 
  > class d =
  >   let x() = new d in
  >   let _y = x() in
  >   object end
  > 
  > class e = object end
  > and f =
  >   let x() = new e in
  >   let _y = x() in
  >   object end
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename illegal_class_expr.ml < illegal_class_expr.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of recursive class expression is not allowed"
      },
      {
        "start": {
          "line": 5,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of recursive class expression is not allowed"
      },
      {
        "start": {
          "line": 9,
          "col": 2
        },
        "end": {
          "line": 10,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of recursive class expression is not allowed"
      },
      {
        "start": {
          "line": 13,
          "col": 2
        },
        "end": {
          "line": 15,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of recursive class expression is not allowed"
      },
      {
        "start": {
          "line": 19,
          "col": 2
        },
        "end": {
          "line": 21,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This kind of recursive class expression is not allowed"
      },
      {
        "start": {
          "line": 23,
          "col": 17
        },
        "end": {
          "line": 23,
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
