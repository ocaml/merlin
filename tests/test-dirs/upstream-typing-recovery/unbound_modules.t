  $ cat >unbound_modules.ml <<'EOF'
  > let x = false
  > let f () =
  >   let open Unknown in
  >   x
  > ;;
  > 
  > let g () =
  >   let open Lsit in
  >   map
  > ;;
  > EOF

  $ $MERLIN single errors -filename unbound_modules.ml < unbound_modules.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 11
        },
        "end": {
          "line": 3,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Unknown"
      },
      {
        "start": {
          "line": 8,
          "col": 11
        },
        "end": {
          "line": 8,
          "col": 15
        },
        "type": "typer",
        "sub": [
          {
            "start": {
              "line": 8,
              "col": 11
            },
            "end": {
              "line": 8,
              "col": 15
            },
            "message": "Hint: Did you mean List?"
          }
        ],
        "valid": true,
        "message": "Unbound module Lsit"
      }
    ],
    "notifications": []
  }
