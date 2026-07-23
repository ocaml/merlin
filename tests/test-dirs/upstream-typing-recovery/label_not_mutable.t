  $ cat >label_not_mutable.ml <<'EOF'
  > type t = { x: int; y: int; mutable z: int}
  > 
  > let f (x: t) =
  >   let () = x.z <- 11 in
  >   let () = x.x <- 12 in
  >   let () = x.y <- 12 in
  >   x
  > 
  > let g x y z = {x; y; z}
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename label_not_mutable.ml < label_not_mutable.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 11
        },
        "end": {
          "line": 5,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The record field x is not mutable"
      },
      {
        "start": {
          "line": 6,
          "col": 11
        },
        "end": {
          "line": 6,
          "col": 20
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The record field y is not mutable"
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
