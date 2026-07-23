  $ cat >not_a_function.ml <<'EOF'
  > let x =
  >   let a : string = 10 in
  >   let () = if (fun x -> x) then () in
  >   a ^ "foo"
  > 
  > module R = struct
  >   let x =
  >     let a : string = 10 in
  >     let () = if (fun x -> x) then () in
  >     a ^ "foo"
  > end
  > 
  > let () = if (fun x -> x) then () else ()
  > 
  > let f () =
  >   String.capitalize_ascii (x ^ R.x)
  > 
  > let t : bool = 0
  > EOF

  $ $MERLIN single errors -filename not_a_function.ml < not_a_function.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 19
        },
        "end": {
          "line": 2,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 3,
          "col": 14
        },
        "end": {
          "line": 3,
          "col": 26
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a function, the expected type is bool
  because it is in the condition of an if-statement"
      },
      {
        "start": {
          "line": 8,
          "col": 21
        },
        "end": {
          "line": 8,
          "col": 23
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      },
      {
        "start": {
          "line": 9,
          "col": 16
        },
        "end": {
          "line": 9,
          "col": 28
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a function, the expected type is bool
  because it is in the condition of an if-statement"
      },
      {
        "start": {
          "line": 13,
          "col": 12
        },
        "end": {
          "line": 13,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a function, the expected type is bool
  because it is in the condition of an if-statement"
      },
      {
        "start": {
          "line": 18,
          "col": 15
        },
        "end": {
          "line": 18,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 0 has type int but an expression was expected of type bool"
      }
    ],
    "notifications": []
  }
