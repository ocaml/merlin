  $ cat >instance_variable_not_mutable.ml <<'EOF'
  > class t = object
  >   val mutable x = 10
  >   val y = 10
  >   method foo : unit =
  >     let () = x <- 11 in
  >     y <- 11
  > end
  > 
  > let _ : int =
  >   let o = object
  >     val x = 10
  >     method foo =
  >       let () = x <- 11 in
  >       12
  >   end
  >   in
  >   o # foo
  > 
  > class u = object
  >   val y = 10
  >   method foo : unit =
  >     let () = y <- y ^ "hello" in
  >     y
  > end
  > 
  > let x : string = 11
  > EOF

  $ $MERLIN single errors -filename instance_variable_not_mutable.ml < instance_variable_not_mutable.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 4
        },
        "end": {
          "line": 6,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable y is not mutable"
      },
      {
        "start": {
          "line": 13,
          "col": 15
        },
        "end": {
          "line": 13,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable x is not mutable"
      },
      {
        "start": {
          "line": 22,
          "col": 13
        },
        "end": {
          "line": 22,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable y is not mutable"
      },
      {
        "start": {
          "line": 22,
          "col": 18
        },
        "end": {
          "line": 22,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value y has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 22,
          "col": 18
        },
        "end": {
          "line": 22,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type string but an expression was expected of type int"
      },
      {
        "start": {
          "line": 23,
          "col": 4
        },
        "end": {
          "line": 23,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This value has type int but an expression was expected of type unit"
      },
      {
        "start": {
          "line": 26,
          "col": 17
        },
        "end": {
          "line": 26,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 11 has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
