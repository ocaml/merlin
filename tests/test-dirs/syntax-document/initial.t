 Merlin should print the typedtree 
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