  $ cat >label_mismatch.ml <<'EOF'
  > type a = {
  >   x : int;
  >   y : int;
  > }
  > 
  > type b = {
  >   x : int;
  >   z : string;
  > }
  > 
  > let _ =
  >   { x = 1; y = 2; z = "oops" }
  > 
  > let x : int = "string"
  > 
  > type d = {
  >   u : int;
  >   v : int;
  > }
  > 
  > type e = {
  >   u : int;
  >   w : string;
  > }
  > 
  > let r = { u = 1; v = 2 }
  > 
  > let _ =
  >   { r with w = "oops" }
  > EOF

  $ $MERLIN single errors -filename label_mismatch.ml < label_mismatch.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 12,
          "col": 11
        },
        "end": {
          "line": 12,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The record field y belongs to the type a
  but is mixed here with fields of type b"
      },
      {
        "start": {
          "line": 14,
          "col": 14
        },
        "end": {
          "line": 14,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      },
      {
        "start": {
          "line": 29,
          "col": 11
        },
        "end": {
          "line": 29,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This record expression is expected to have type d
  There is no field w within type d"
      }
    ],
    "notifications": []
  }
