  $ cat >literal_overflow.ml <<'EOF'
  > let t = 99999999999999999999999999999999999999999999999999999999999
  > let u = 99999999999999999999999999999999999999999999999999999999999l
  > let k = 99999999999999999999999999999999999999999999999999999999999L
  > 
  > let r =
  >   let (+) = Int64.add in
  >   Int64.(of_int t + of_int32 u + k)
  > 
  > let s : string = r
  > EOF

  $ $MERLIN single errors -filename literal_overflow.ml < literal_overflow.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 67
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Integer literal exceeds the range of representable integers of type int"
      },
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 68
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Integer literal exceeds the range of representable integers of type int32"
      },
      {
        "start": {
          "line": 3,
          "col": 8
        },
        "end": {
          "line": 3,
          "col": 68
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Integer literal exceeds the range of representable integers of type int64"
      },
      {
        "start": {
          "line": 9,
          "col": 17
        },
        "end": {
          "line": 9,
          "col": 18
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "The value r has type int64 but an expression was expected of type string"
      }
    ],
    "notifications": []
  }
