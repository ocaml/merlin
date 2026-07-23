  $ cat >incoherent_label_order.ml <<'EOF'
  > let g f = f ~x:0 ~y:0; f ~y:0 ~x:0
  > let t : int = ""
  > EOF

  $ $MERLIN single errors -filename incoherent_label_order.ml < incoherent_label_order.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 23
        },
        "end": {
          "line": 1,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This function is applied to arguments in an order different from other calls.
  This is only allowed when the real type is known."
      },
      {
        "start": {
          "line": 2,
          "col": 14
        },
        "end": {
          "line": 2,
          "col": 16
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This constant has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
