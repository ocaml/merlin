  $ cat >wrong_name.ml <<'EOF'
  > type person = { name : string; age : int }
  > 
  > let _ =
  >   let p = { name = "Alice"; age = 30 } in
  >   p.height
  > 
  > type t =
  >   | A of { foo: int; bar: string }
  > 
  > let f (A {foo; bar; baz}) = ()
  > 
  > let x = 10
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename wrong_name.ml < wrong_name.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type person
  There is no field height within type person"
      },
      {
        "start": {
          "line": 10,
          "col": 20
        },
        "end": {
          "line": 10,
          "col": 23
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 10,
              "col": 20
            },
            "end": {
              "line": 10,
              "col": 23
            },
            "message": "Hint: Did you mean bar?"
          }
        ],
        "valid": true,
        "message": "The field baz is not part of the record argument for the t.A constructor"
      },
      {
        "start": {
          "line": 14,
          "col": 17
        },
        "end": {
          "line": 14,
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
