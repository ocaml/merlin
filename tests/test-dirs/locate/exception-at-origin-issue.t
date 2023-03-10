  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > exception Unix_error = Unix.Unix_error
  > EOF

  $ dune build 

FIXME: Merlin should jump to stdlib Unix.Unix_error
  $ $MERLIN single locate -look-for ml -position 1:31 \
  > -filename foo.ml <foo.ml
  {
    "class": "return",
    "value": "Already at definition point",
    "notifications": []
  }

FIXME: Merlin should jump to stdlib Unix
  $ $MERLIN single locate -look-for ml -position 1:25 \
  > -filename foo.ml <foo.ml
  {
    "class": "return",
    "value": "Already at definition point",
    "notifications": []
  }
