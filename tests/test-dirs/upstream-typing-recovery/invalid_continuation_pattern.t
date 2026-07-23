  $ cat >invalid_continuation_pattern.ml <<'EOF'
  > type _ eff += A: 'a -> int eff
  > 
  > let handle_1 f =
  >   match f () with
  >   | "foo" -> "foo"
  >   | x -> x
  >   | effect A, [k] -> "bar"
  > 
  > let handle_2 f =
  >   match f () with
  >   | "foo" -> "foo"
  >   | x -> x
  >   | effect A, (Some k) -> "bar"
  > 
  > let t = (handle_1 (Fun.const "foo")) ^ (handle_2 (Fun.const "bar"))
  > 
  > let u : int = t
  > EOF

  $ $MERLIN single errors -filename invalid_continuation_pattern.ml < invalid_continuation_pattern.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 14
        },
        "end": {
          "line": 7,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid continuation pattern: only variables and _ are allowed ."
      },
      {
        "start": {
          "line": 13,
          "col": 14
        },
        "end": {
          "line": 13,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid continuation pattern: only variables and _ are allowed ."
      },
      {
        "start": {
          "line": 17,
          "col": 14
        },
        "end": {
          "line": 17,
          "col": 15
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value t has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
