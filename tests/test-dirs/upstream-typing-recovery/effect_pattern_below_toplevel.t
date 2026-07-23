  $ cat >effect_pattern_below_toplevel.ml <<'EOF'
  > type _ eff += A: 'a -> int eff
  > 
  > let handle_1 a =
  >   match a () with
  >   | _ -> "foo"
  >   | Some (effect A, _) -> "bar"
  > 
  > let handle_2 a =
  >   match a () with
  >   | _ -> "baz"
  >   | [effect A, _] -> "foobar"
  > 
  > let t =
  >   let a = handle_1 (fun _ -> None)
  >   and b = handle_2 (fun _ -> [])
  >   in a ^ b
  > 
  > 
  > let k : int = t + t
  > EOF

  $ $MERLIN single errors -filename effect_pattern_below_toplevel.ml < effect_pattern_below_toplevel.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 9
        },
        "end": {
          "line": 6,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Effect patterns must be at the top level of a match case."
      },
      {
        "start": {
          "line": 11,
          "col": 5
        },
        "end": {
          "line": 11,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Effect patterns must be at the top level of a match case."
      },
      {
        "start": {
          "line": 19,
          "col": 14
        },
        "end": {
          "line": 19,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value t has type string but an expression was expected of type int"
      },
      {
        "start": {
          "line": 19,
          "col": 18
        },
        "end": {
          "line": 19,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value t has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
