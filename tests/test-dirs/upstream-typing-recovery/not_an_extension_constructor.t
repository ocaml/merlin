  $ cat >not_an_extension_constructor.ml <<'EOF'
  > type t = A | B | C
  > let x = [%extension_constructor A]
  > let f =
  >   let a = [%extension_constructor B]
  >   and b = [%extension_constructor C] in
  >   (a, b)
  > 
  > let u = (snd f, fst f, x)
  > let t : int = u
  > EOF

  $ $MERLIN single errors -filename not_an_extension_constructor.ml < not_an_extension_constructor.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 32
        },
        "end": {
          "line": 2,
          "col": 33
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constructor is not an extension constructor."
      },
      {
        "start": {
          "line": 4,
          "col": 34
        },
        "end": {
          "line": 4,
          "col": 35
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constructor is not an extension constructor."
      },
      {
        "start": {
          "line": 5,
          "col": 34
        },
        "end": {
          "line": 5,
          "col": 35
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constructor is not an extension constructor."
      },
      {
        "start": {
          "line": 9,
          "col": 14
        },
        "end": {
          "line": 9,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value u has type 'a * 'b * 'c but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
