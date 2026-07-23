  $ cat >optional_poly_param.ml <<'EOF'
  > let id x = x
  > 
  > let a ?(id : 'a. 'a -> 'a) () = id 3, id "three"
  > let b ?(id : 'a. int -> int) () = id 3
  > 
  > let r =
  >   let x = fst (a ~id ())
  >   and y = (b ~id ()) in
  >   x + y
  > 
  > let t : string = r
  > EOF

  $ $MERLIN single errors -filename optional_poly_param.ml < optional_poly_param.ml
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
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The optional parameter id cannot have a polymorphic type."
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The optional parameter id cannot have a polymorphic type."
      },
      {
        "start": {
          "line": 7,
          "col": 18
        },
        "end": {
          "line": 7,
          "col": 20
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      },
      {
        "start": {
          "line": 7,
          "col": 21
        },
        "end": {
          "line": 7,
          "col": 23
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      },
      {
        "start": {
          "line": 8,
          "col": 14
        },
        "end": {
          "line": 8,
          "col": 16
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      },
      {
        "start": {
          "line": 8,
          "col": 17
        },
        "end": {
          "line": 8,
          "col": 19
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 20: this argument will not be used by the function."
      },
      {
        "start": {
          "line": 11,
          "col": 17
        },
        "end": {
          "line": 11,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value r has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
