  $ cat >log_or_raise.ml <<'EOF'
  > (* Multiply_bound_variable. *)
  > let f = function
  > | Some x, Some x | [], x -> x
  > EOF

  $ $MERLIN single errors -filename log_or_raise.ml < log_or_raise.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The variable x on the left-hand side of this or-pattern has type 'a
  but on the right-hand side it has type 'a option
  The type variable 'b occurs inside 'b option"
      },
      {
        "start": {
          "line": 3,
          "col": 15
        },
        "end": {
          "line": 3,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Variable x is bound several times in this matching"
      },
      {
        "start": {
          "line": 3,
          "col": 19
        },
        "end": {
          "line": 3,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This variant pattern is expected to have type 'a option
  There is no constructor [] within type option"
      },
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      }
    ],
    "notifications": []
  }
