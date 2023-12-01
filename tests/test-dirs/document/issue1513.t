Merlin should show comments for a type's constructor from another module:
  $ cat >naux.ml <<EOF
  > type t =
  >   | A
  >   (**A Comment *)
  >   | B
  >   (**B Comment *)
  >   | C
  > EOF

  $ cat >main.ml <<EOF
  > let _ = Naux.A
  > let _ = Naux.B
  > let _ = Naux.C
  > EOF

  $ $OCAMLC -c -bin-annot naux.ml

FIXME: We should not rely on "fallbacking". This requires a compiler change.
  $ $MERLIN single document -position 1:13 \
  > -log-file - -log-section locate \
  > -filename main.ml <main.ml 2>&1 | 
  > grep "Uid not found in the table."
  Uid not found in the table.

FIXME: expected "B Comment"
  $ $MERLIN single document -position 2:13 \
  > -filename main.ml <main.ml | tr '\n' ' ' | jq '.value'
  "A Comment B Comment"

FIXME
  $ $MERLIN single document -position 3:13 \
  > -filename main.ml <main.ml | jq '.value'
  "B Comment"

  $ rm naux.cmt

Merlin should show comments for a type's constructor from the current module:
  $ cat >main.ml <<EOF
  > type t =
  >   | A
  >   (**A Comment *)
  >   | B
  >   (**B Comment *)
  >   | C
  > EOF

  $ $MERLIN single document -position 2:4 \
  > -filename main.ml <main.ml | jq '.value'
  "A Comment"

  $ $MERLIN single document -position 4:4 \
  > -filename main.ml <main.ml | tr '\n' ' ' | jq '.value'
  "B Comment"

  $ $MERLIN single document -position 6:4 \
  > -filename main.ml <main.ml | jq '.value'
  "No documentation available"
