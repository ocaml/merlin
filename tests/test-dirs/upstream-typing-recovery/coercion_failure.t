  $ cat >coercion_failure.ml <<'EOF'
  > module R = struct
  >   let f =
  >     let x = ([`B] :> [`A]) in
  >     `A :: x
  > 
  >   let r = `A :: f
  >   let t : string = 10
  > end
  > 
  > let t : string = 10
  > let _ = t ^ R.t
  > EOF

  $ $MERLIN single errors -filename coercion_failure.ml < coercion_failure.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 13
        },
        "end": {
          "line": 3,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression cannot be coerced to type [ `A ]; it has type [> `B ] list
  but is here used with type [< `A ]"
      },
      {
        "start": {
          "line": 7,
          "col": 19
        },
        "end": {
          "line": 7,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 10,
          "col": 17
        },
        "end": {
          "line": 10,
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
