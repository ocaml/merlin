 Merlin should print some documentation
  $ cat >doc.ml <<EOF
  > type t = ..;
  > EOF

  $ $MERLIN single syntax-document -position 1:1 \
  > -filename doc.ml < doc.ml

  $ cat >doc.ml <<EOF
  > type t = A; 
  > EOF

  $ $MERLIN single syntax-document -position 1:1 \
  > -filename doc.ml < doc.ml

  $ cat >doc.ml <<EOF
  > let t = 2;
  > EOF

  $ $MERLIN single syntax-document -position 1:1 \
  > -filename doc.ml < doc.ml

  $ cat >doc.ml <<EOF
  > type t = |;
  > EOF

  $ $MERLIN single syntax-document -position 1:1 \
  > -filename doc.ml < doc.ml
