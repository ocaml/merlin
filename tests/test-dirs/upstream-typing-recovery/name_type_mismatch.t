  $ cat >name_type_mismatch.ml <<'EOF'
  > module M = struct type t = { x: int } end
  > type t = { z : int }
  > let x = ({ M.x = 0 }: t)
  > 
  > let x = 10
  > 
  > let t : string = 10
  > EOF

  $ $MERLIN single errors -filename name_type_mismatch.ml < name_type_mismatch.ml
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
          "col": 14
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The field M.x belongs to the record type M.t
  but a field was expected belonging to the record type t"
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
