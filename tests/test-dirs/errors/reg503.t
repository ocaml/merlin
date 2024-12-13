  $ cat >test.ml <<'EOF'
  > class test _a =
  > object
  >   method b x = x
  > end
  > EOF

FIXME: Type variable are not shared between the two parts of the error message:
  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 4,
          "col": 3
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Some type variables are unbound in this type:
    class test : 'a -> object method b : 'b -> 'b end
  The method b has type 'b -> 'b where 'b is unbound"
      }
    ],
    "notifications": []
  }
