  $ cat >modules_not_allowed.ml <<'EOF'
  > let (module M) = 0
  > 
  > let c =
  >   let module M = struct
  >     let x = 10
  > 
  >     class t =
  >       let (module Ob) = m in
  >       object end
  >   end
  >   in M.x + 1
  > 
  > let t : int = true
  > EOF

  $ $MERLIN single errors -filename modules_not_allowed.ml < modules_not_allowed.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 4
        },
        "end": {
          "line": 1,
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Modules are not allowed in this pattern."
      },
      {
        "start": {
          "line": 8,
          "col": 10
        },
        "end": {
          "line": 8,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Modules are not allowed in this pattern."
      },
      {
        "start": {
          "line": 8,
          "col": 24
        },
        "end": {
          "line": 8,
          "col": 25
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value m"
      },
      {
        "start": {
          "line": 13,
          "col": 14
        },
        "end": {
          "line": 13,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a boolean literal, the expected type is int"
      }
    ],
    "notifications": []
  }
