  $ cat >letop_andop_type_clash.ml <<'EOF'
  > module A = struct
  >   let (let+) = 7
  >   let (and+) = not
  > 
  >   let ill_typed =
  >     let+ x = 1 and+ y = 11 in x + y
  > end
  > 
  > module F = struct
  >   let (let+) x f = f x
  >   let (and+) = not
  > 
  >   let ill_typed =
  >     let+ x = 1 and+ y = 11 in x + y
  > end
  > EOF

  $ $MERLIN single errors -filename letop_andop_type_clash.ml < letop_andop_type_clash.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 4
        },
        "end": {
          "line": 6,
          "col": 8
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The operator let+ has type int but it was expected to have type
    'a -> ('b * 'c -> 'd) -> 'e"
      },
      {
        "start": {
          "line": 6,
          "col": 15
        },
        "end": {
          "line": 6,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The operator and+ has type bool -> bool but it was expected to have type
    bool -> 'a -> 'b
  Type bool is not compatible with type 'a -> 'b"
      },
      {
        "start": {
          "line": 14,
          "col": 15
        },
        "end": {
          "line": 14,
          "col": 19
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The operator and+ has type bool -> bool but it was expected to have type
    bool -> 'a -> 'b
  Type bool is not compatible with type 'a -> 'b"
      }
    ],
    "notifications": []
  }
