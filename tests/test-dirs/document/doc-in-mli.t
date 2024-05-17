  $ cat >main.mli <<'EOF'
  > (** A great module *)
  > 
  > val x : int
  > (** The only answer *)
  > EOF

  $ cat >main.ml <<'EOF'
  > let x = 42
  > let _ = x
  > EOF

  $ cat >lib.ml <<'EOF'
  > let _ = Main.x
  > EOF

  $ cat >.merlin << 'EOF'
  > B .
  > S .
  > EOF

  $ $OCAMLC -c -bin-annot main.mli main.ml lib.ml

  $ ls
  lib.cmi
  lib.cmo
  lib.cmt
  lib.ml
  main.cmi
  main.cmo
  main.cmt
  main.cmti
  main.ml
  main.mli

FIXME: Querying for doc from the implementation for values defined in the
current compilation unit does not work because merlin cannot link the
declarations coming from the mli and the ml file:
  $ $MERLIN single document -position 1:4 -filename main.ml < main.ml
  {
    "class": "return",
    "value": "No documentation available",
    "notifications": []
  }

  $ $MERLIN single document -position 2:8 -filename main.ml < main.ml
  {
    "class": "return",
    "value": "No documentation available",
    "notifications": []
  }

Querying from the mli itself work as expected, but is not very useful:
  $ $MERLIN single document -position 3:4 -filename main.mli < main.mli
  {
    "class": "return",
    "value": "The only answer",
    "notifications": []
  }

Querying from another unit work as expected:
  $ $MERLIN single document -position 1:13 -filename lib.ml < lib.ml
  {
    "class": "return",
    "value": "The only answer",
    "notifications": []
  }
