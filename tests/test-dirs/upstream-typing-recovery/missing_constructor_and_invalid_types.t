FIXME Divergence from the compiler reference: Merlin reports the generic "This constant
has type ..." instead of naming the literal ("The constant 3 / 'a' has type
..."). (see claude-report.md)

  $ cat >missing_constructor_and_invalid_types.ml <<'EOF'
  > type t = A | B
  > 
  > let f (C : t) : int = ()
  > 
  > let f (x : t) =
  >   match x with
  >   | A -> ()
  >   | B -> 3
  >   | C -> 'a'
  > EOF

  $ $MERLIN single errors -filename missing_constructor_and_invalid_types.ml < missing_constructor_and_invalid_types.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 7
        },
        "end": {
          "line": 3,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type t
  There is no constructor C within type t"
      },
      {
        "start": {
          "line": 3,
          "col": 22
        },
        "end": {
          "line": 3,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a unit literal, the expected type is int"
      },
      {
        "start": {
          "line": 8,
          "col": 9
        },
        "end": {
          "line": 8,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type int but an expression was expected of type unit"
      },
      {
        "start": {
          "line": 9,
          "col": 4
        },
        "end": {
          "line": 9,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type t
  There is no constructor C within type t"
      },
      {
        "start": {
          "line": 9,
          "col": 9
        },
        "end": {
          "line": 9,
          "col": 12
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type char but an expression was expected of type unit"
      }
    ],
    "notifications": []
  }
