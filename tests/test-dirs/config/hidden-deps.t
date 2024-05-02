
  $ cat >main.ml <<'EOF'
  > let _ = print_int Vlib.x
  > EOF

  $ mkdir hsrc
  $ cat >hsrc/hlib.ml <<'EOF'
  > let x = 42
  > EOF

  $ mkdir vsrc
  $ cat >vsrc/vlib.ml <<'EOF'
  > include Hlib
  > EOF

  $ mkdir _build
  $ mkdir _build/hsrc
  $ mkdir _build/vsrc

  $ cp main.ml _build/main.ml
  $ cp hsrc/hlib.ml _build/hsrc/hlib.ml
  $ cp vsrc/vlib.ml _build/vsrc/vlib.ml

  $ (cd _build/hsrc && $OCAMLC -c -bin-annot hlib.ml )
  $ (cd _build/vsrc && $OCAMLC -c -bin-annot vlib.ml -I ../hsrc )
  $ (cd _build && $OCAMLC -c -bin-annot main.ml -I vsrc)

  $ cat >.merlin <<'EOF'
  > B _build
  > B _build/vsrc
  > BH _build/hsrc
  > S .
  > S vsrc
  > SH hsrc
  > EOF

  $ $MERLIN single errors -filename main.ml <main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 0,
          "col": -1
        },
        "end": {
          "line": 0,
          "col": -1
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Stdlib"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound value print_int"
      },
      {
        "start": {
          "line": 1,
          "col": 18
        },
        "end": {
          "line": 1,
          "col": 24
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module Vlib"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 1:23 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "Not in environment 'Vlib.x'",
    "notifications": []
  }
