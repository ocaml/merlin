  $ cat >undefined_self_method.ml <<'EOF'
  > let _ =
  >   (object(self)
  >     method foo = self # bar
  >   end) # foo
  > 
  > let _ =
  >   (object(self)
  >     method bar = object
  >       method foo = 10
  >       end
  >     method foo = self # bar
  >   end) # foo # foo + 10
  > 
  > let t : bool = 10
  > EOF

  $ $MERLIN single errors -filename undefined_self_method.ml < undefined_self_method.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 17
        },
        "end": {
          "line": 3,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has no method bar"
      },
      {
        "start": {
          "line": 14,
          "col": 15
        },
        "end": {
          "line": 14,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type bool"
      }
    ],
    "notifications": []
  }
