  $ cat >test.ml <<'EOF'
  > type foo = {
  >   bar: X.t;
  > }
  > type foo2 = X.t
  > type foo3 = bar
  > EOF

FIXME:Merlin should not report unbound variable errors in that case since it is
due to it's own type recovery.
  $ $MERLIN single errors -filename test.ml <test.ml 
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 3,
          "col": 1
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "A type variable is unbound in this type declaration.
  In field bar: 'a the variable 'a is unbound"
      },
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module X"
      },
      {
        "start": {
          "line": 4,
          "col": 0
        },
        "end": {
          "line": 4,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "A type variable is unbound in this type declaration.
  In type 'a the variable 'a is unbound"
      },
      {
        "start": {
          "line": 4,
          "col": 12
        },
        "end": {
          "line": 4,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module X"
      },
      {
        "start": {
          "line": 5,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "A type variable is unbound in this type declaration.
  In type 'a the variable 'a is unbound"
      },
      {
        "start": {
          "line": 5,
          "col": 12
        },
        "end": {
          "line": 5,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound type constructor bar"
      }
    ],
    "notifications": []
  }
