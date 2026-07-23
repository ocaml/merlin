  $ cat >unbound_instance_variable.ml <<'EOF'
  > class c = object
  >   val x = 0
  >   method m: c = {< y=1 >}
  > end
  > 
  > class e = object
  >   val x = 0
  >   method m: c = {< x=1; k=10 >}
  > end
  > 
  > class f = object
  >   val x = 10
  >   method a : f = {<x = 10>}
  >   method b : f = {<x = 10; y = 12>}
  >   method c : f = {<k = 15>}
  > end
  > 
  > let _ =
  >   let o = object
  >     val x = 10
  >       method a = {< x = 10>}
  >     end
  >   and _ = object
  >     val x = 11
  >     method foo = x + 1
  >     method b e = {<x = (e # a); y = 22>}
  >   end
  >   in (o # a)
  > EOF

  $ $MERLIN single errors -filename unbound_instance_variable.ml < unbound_instance_variable.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 16
        },
        "end": {
          "line": 3,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound instance variable y"
      },
      {
        "start": {
          "line": 8,
          "col": 16
        },
        "end": {
          "line": 8,
          "col": 31
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound instance variable k"
      },
      {
        "start": {
          "line": 13,
          "col": 17
        },
        "end": {
          "line": 13,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type < a : f; b : f; c : f; .. >
  but an expression was expected of type < .. >
  Self type cannot escape its class"
      },
      {
        "start": {
          "line": 14,
          "col": 17
        },
        "end": {
          "line": 14,
          "col": 35
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound instance variable y"
      },
      {
        "start": {
          "line": 15,
          "col": 17
        },
        "end": {
          "line": 15,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound instance variable k"
      },
      {
        "start": {
          "line": 26,
          "col": 17
        },
        "end": {
          "line": 26,
          "col": 40
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound instance variable y"
      }
    ],
    "notifications": []
  }
