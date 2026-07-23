  $ cat >partial_tuple_pattern_bad_type.ml <<'EOF'
  > let f (z, (~y, ~x, ..)) = x, y, z
  > let g (~x, ~y, ..) = x, y
  > 
  > let x =
  >   let (a, b, c) = f (10, (~y:11, ~x:12)) in
  >   let (d, e) = g (~x:22, ~y:33) in
  >   a + b + c + d + e
  > EOF

  $ $MERLIN single errors -filename partial_tuple_pattern_bad_type.ml < partial_tuple_pattern_bad_type.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Could not determine the type of this partial tuple pattern."
      },
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Could not determine the type of this partial tuple pattern."
      }
    ],
    "notifications": []
  }
