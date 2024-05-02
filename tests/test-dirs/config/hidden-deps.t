
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
    "value": [],
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 1:23 -filename main.ml <main.ml
  {
    "class": "return",
    "value": "'Vlib.x' seems to originate from 'Hlib' whose ML file could not be found",
    "notifications": []
  }
