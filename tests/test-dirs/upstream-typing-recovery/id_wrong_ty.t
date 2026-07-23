  $ cat >id_wrong_ty.ml <<'EOF'
  > let rec id_wrong_ty : int = fun x -> x
  > let f () = id_wrong_ty + 11
  > EOF

  $ $MERLIN single errors -filename id_wrong_ty.ml < id_wrong_ty.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 28
        },
        "end": {
          "line": 1,
          "col": 38
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression should not be a function, the expected type is int"
      }
    ],
    "notifications": []
  }
