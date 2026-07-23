  $ cat >invalid_interval_and_invalid_for_loop_index.ml <<'EOF'
  > let foo x = function
  >   | 0 .. 10 -> ()
  >   | _ -> begin match x with
  >       | "foo" .. "bar" -> ()
  >       | _ ->
  >           for (Some i) = 1 to 10 do
  >             print_endline "Message"
  >           done
  >     end
  > 
  > let f () = 10
  > 
  > let g = f (foo "foo" 10)
  > EOF

  $ $MERLIN single errors -filename invalid_interval_and_invalid_for_loop_index.ml < invalid_interval_and_invalid_for_loop_index.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 4
        },
        "end": {
          "line": 2,
          "col": 5
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only character intervals are supported in patterns."
      },
      {
        "start": {
          "line": 4,
          "col": 8
        },
        "end": {
          "line": 4,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Only character intervals are supported in patterns."
      },
      {
        "start": {
          "line": 6,
          "col": 14
        },
        "end": {
          "line": 6,
          "col": 22
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid for-loop index: only variables and _ are allowed."
      }
    ],
    "notifications": []
  }
