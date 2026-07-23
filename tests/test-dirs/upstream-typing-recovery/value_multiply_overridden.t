  $ cat >value_multiply_overridden.ml <<'EOF'
  > class a = object(self : 'a)
  >   val x = 10
  >   method foo : 'a = {<x = 11; x = 12>}
  > end
  > 
  > class b = object
  >   inherit a
  >   method! foo = {< x = 12; x = 13 >}
  > end
  > 
  > let f =
  >   let o =
  >     object
  >       inherit a
  >       method! foo = {< x = 12; x = 13 >}
  >     end
  >   in o # foo
  > 
  > module R = struct
  >   let f =
  >     let o =
  >       object
  >         inherit a
  >         method! foo = {< x = 12; x = 13 >}
  >       end
  >     in o # foo
  > end
  > 
  > let x = (R.f ) # foo
  > let t : string = 11
  > EOF

  $ $MERLIN single errors -filename value_multiply_overridden.ml < value_multiply_overridden.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 20
        },
        "end": {
          "line": 3,
          "col": 38
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable x is overridden several times"
      },
      {
        "start": {
          "line": 8,
          "col": 16
        },
        "end": {
          "line": 8,
          "col": 36
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable x is overridden several times"
      },
      {
        "start": {
          "line": 15,
          "col": 20
        },
        "end": {
          "line": 15,
          "col": 40
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable x is overridden several times"
      },
      {
        "start": {
          "line": 24,
          "col": 22
        },
        "end": {
          "line": 24,
          "col": 42
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The instance variable x is overridden several times"
      },
      {
        "start": {
          "line": 30,
          "col": 17
        },
        "end": {
          "line": 30,
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
