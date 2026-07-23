  $ cat >missing_errors_in_pattern.ml <<'EOF'
  > type i = I of int
  > 
  > let f = function
  >   | I "foo" ->
  >       3.14 + 2
  > 
  > let f = function
  >   | J ->
  >       3.14 + 2
  > 
  > let f = function
  >   | `azdwbie
  >   | `c7diagq ->
  >       3.14 + 2
  > EOF

  $ $MERLIN single errors -filename missing_errors_in_pattern.ml < missing_errors_in_pattern.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 6
        },
        "end": {
          "line": 4,
          "col": 11
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern matches values of type string
  but a pattern was expected which matches values of type int"
      },
      {
        "start": {
          "line": 5,
          "col": 6
        },
        "end": {
          "line": 5,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 3.14 has type float but an expression was expected of type int"
      },
      {
        "start": {
          "line": 8,
          "col": 4
        },
        "end": {
          "line": 8,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound constructor J"
      },
      {
        "start": {
          "line": 9,
          "col": 6
        },
        "end": {
          "line": 9,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 3.14 has type float but an expression was expected of type int"
      },
      {
        "start": {
          "line": 13,
          "col": 4
        },
        "end": {
          "line": 13,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variant tags `azdwbie and `c7diagq have the same hash value.
  Change one of them."
      },
      {
        "start": {
          "line": 14,
          "col": 6
        },
        "end": {
          "line": 14,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 3.14 has type float but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
