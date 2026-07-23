  $ cat >extra_missing_tuple_label.ml <<'EOF'
  > let lt = ~x:1, 2, ~y:3, ~z:4, 5, 6
  > 
  > let a =
  >   let ~x, k1, ~y, ~z, ~w, k2, k3 = lt in
  >   ()
  > 
  > let b =
  >   let ~x, k1, ~y, ~w, k2, k3 = lt in
  >   ()
  > 
  > let d =
  >   let ~x, k1, ~y, ~z, k2, k3, k4 = lt in
  >   x, y, z
  > 
  > type t = x:int * bool * x:int
  > 
  > let _ = 1, ~x:2, 3, ~x:4
  > 
  > let f (a, ~x, b, ~x:c) = ()
  > EOF

  $ $MERLIN single errors -filename extra_missing_tuple_label.ml < extra_missing_tuple_label.ml
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
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern was expected to match values of type
  x:int * int * y:int * z:int * int * int,
  but it contains an extra component with label w."
      },
      {
        "start": {
          "line": 8,
          "col": 6
        },
        "end": {
          "line": 8,
          "col": 28
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern was expected to match values of type
  x:int * int * y:int * z:int * int * int,
  but it is missing a component with label z.
  Hint: use .. to ignore some components."
      },
      {
        "start": {
          "line": 12,
          "col": 6
        },
        "end": {
          "line": 12,
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This pattern was expected to match values of type
  x:int * int * y:int * z:int * int * int,
  but it contains an extra unlabeled component."
      },
      {
        "start": {
          "line": 13,
          "col": 2
        },
        "end": {
          "line": 13,
          "col": 3
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value x"
      },
      {
        "start": {
          "line": 13,
          "col": 5
        },
        "end": {
          "line": 13,
          "col": 6
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value y"
      },
      {
        "start": {
          "line": 13,
          "col": 8
        },
        "end": {
          "line": 13,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value z"
      },
      {
        "start": {
          "line": 15,
          "col": 9
        },
        "end": {
          "line": 15,
          "col": 29
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This tuple type has two labels named x"
      },
      {
        "start": {
          "line": 17,
          "col": 8
        },
        "end": {
          "line": 17,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This tuple expression has two labels named x"
      },
      {
        "start": {
          "line": 19,
          "col": 6
        },
        "end": {
          "line": 19,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This tuple pattern has two labels named x"
      }
    ],
    "notifications": []
  }
