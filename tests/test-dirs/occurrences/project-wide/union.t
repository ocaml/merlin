Serialization of `related_uids` requires special cares as the union-find
algorithm relies on deserialization preserving physical identity (for mutations
to work).  The issue manifested on sufficiently large indexes (if small, then
the marshal wouldn't be granular):

  $ NB=1024
  $ for i in $(seq 1 $NB); do echo "let x$i = 0"; done >test.ml
  $ for i in $(seq 1 $NB); do echo "val x$i : int"; done >test.mli
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c test.mli test.ml

A signature containing the same symbols:

  $ echo "module type S = sig\n$(cat test.mli)\nend" >sig.ml
  $ echo "module type S = sig\n$(cat test.mli)\nend" >sig.mli
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c sig.mli sig.ml

At this point `ŧest` and `sig` are unrelated. We'll later force their unification with:

  $ cat >both.ml <<EOF
  > module M = (Test : Sig.S)
  > EOF
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c both.ml

  $ cat > .merlin << EOF
  > INDEX project.ocaml-index
  > SOURCE_ROOT .
  > EOF

First compute the index for `test` and `sig`:

  $ ocaml-index aggregate test.cmti test.cmt sig.cmti sig.cmt --root . --rewrite-root
  $ mv project.ocaml-index test_sig.ocaml-index

Then for `both`:

  $ ocaml-index aggregate both.cmt --root . --rewrite-root

Merge everything together, which reveals the relation between `test` and `sig` uids:

  $ ocaml-index aggregate test_sig.ocaml-index project.ocaml-index

All files should be listed on queries: (except `both.ml`)

  $ $MERLIN single occurrences -scope renaming -identifier-at 1:5 -filename test.ml < test.ml | jq '.value[] | .file'
  "$TESTCASE_ROOT/test.ml"
  "$TESTCASE_ROOT/sig.ml"
  "$TESTCASE_ROOT/sig.mli"
  "$TESTCASE_ROOT/test.mli"

  $ $MERLIN single occurrences -scope renaming -identifier-at 50:5 -filename test.ml < test.ml | jq '.value[] | .file'
  "$TESTCASE_ROOT/test.ml"
  "$TESTCASE_ROOT/sig.ml"
  "$TESTCASE_ROOT/sig.mli"
  "$TESTCASE_ROOT/test.mli"
