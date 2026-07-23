  $ cat >invalid_atomic_loc_payload.ml <<'EOF'
  > module E = struct
  >   type t = { mutable x : int [@atomic] }
  >   let f t = [%atomic.loc t]
  > end
  > 
  > let _ = E.f None
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename invalid_atomic_loc_payload.ml < invalid_atomic_loc_payload.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 12
        },
        "end": {
          "line": 3,
          "col": 27
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Invalid [%atomic.loc] payload, a record field access is expected."
      },
      {
        "start": {
          "line": 7,
          "col": 17
        },
        "end": {
          "line": 7,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The constant 10 has type int but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
