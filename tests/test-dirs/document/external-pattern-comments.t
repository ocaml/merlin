  $ cat >main.ml <<EOF
  > let _ = Lib.y
  > EOF

  $ cat >lib.ml <<EOF
  > (** doc for all node *)
  > let x, y = 2, 3
  > EOF

  $ $OCAMLC -c -bin-annot lib.ml main.ml

  $ $MERLIN single document -position 1:12 \
  > -log-file - 2>log \
  > -filename main.ml <main.ml 
  {
    "class": "return",
    "value": "doc for all node",
    "notifications": []
  }

We should not rely on the heuristic to get that comment
  $ cat log | grep -A 2 "looking around"
  [1]
