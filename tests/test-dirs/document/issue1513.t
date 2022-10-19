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

FIXME: the constructors are missing from the [uid_to_loc] tables
  $ $MERLIN single document -position 1:13 \
  > -filename main.ml <main.ml | jq '.value'
  "didn't manage to find Naux.t"

  $ $MERLIN single document -position 2:13 \
  > -filename main.ml <main.ml | tr '\n' ' ' | jq '.value'
  "didn't manage to find Naux.t"

  $ $MERLIN single document -position 3:13 \
  > -filename main.ml <main.ml | jq '.value'
  "didn't manage to find Naux.t"

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

FIXME: expected "B Comment"
  $ $MERLIN single document -position 4:4 \
  > -filename main.ml <main.ml | tr '\n' ' ' | jq '.value'
  "A Comment B Comment"

FIXME: expected ""
  $ $MERLIN single document -position 6:4 \
  > -filename main.ml <main.ml | jq '.value'
  "B Comment"

The issue probaby lies in the heuristics in [ocamldoc.ml]
