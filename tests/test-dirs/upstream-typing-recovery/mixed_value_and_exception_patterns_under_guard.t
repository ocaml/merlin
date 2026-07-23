  $ cat >mixed_value_and_exception_patterns_under_guard.ml <<'EOF'
  > let f g =
  >   match (g ()) with
  >   | Some x | exception Failure x when String.equal x "foo" ->
  >       "succeed"
  >   | _ -> "failure"
  > 
  > let h g x =
  >   let a = f g in
  >   let b = match x with
  >     | Some "foo" -> "foo"
  >     | None -> "erf"
  >     | Some _ | exception Not_found when a = "foo" -> ""
  >   in a ^ b
  > 
  > let t = (h (fun ()-> Some "foo") None) ^ (f (fun () -> None))
  > let u = t ^ t
  > 
  > let k = u + 10
  > EOF

  $ $MERLIN single errors -filename mixed_value_and_exception_patterns_under_guard.ml < mixed_value_and_exception_patterns_under_guard.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 32
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Mixing value and exception patterns under when-guards is not supported."
      },
      {
        "start": {
          "line": 12,
          "col": 6
        },
        "end": {
          "line": 12,
          "col": 34
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Mixing value and exception patterns under when-guards is not supported."
      },
      {
        "start": {
          "line": 18,
          "col": 8
        },
        "end": {
          "line": 18,
          "col": 9
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value u has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
