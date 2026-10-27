Test the --standalone flag.

Setup a simple project.

  $ cat > foo.ml <<EOF
  > let foo = "foo"
  > EOF

  $ cat > a.ml <<EOF
  > let foo = Foo.foo
  > EOF

  $ cat > b.ml <<EOF
  > let foo = Foo.foo
  > EOF

  $ cat > c.ml <<EOF
  > let foo = Foo.foo
  > EOF

Compile the project.

  $ $OCAMLC -bin-annot -bin-annot-occurrences -c foo.ml
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c a.ml
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c b.ml
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c c.ml

Create some index files. We make an index for each ml file. We also make an
index called AB, which points to the A and B indexes. Lastly, we aggregate the
AB and C indexes into a single index. We do a few mixtures of standalone vs
non-standalone indices.

  $ ocaml-index aggregate -o a.idx a.cmt
  $ ocaml-index aggregate -o b.idx b.cmt
  $ ocaml-index aggregate -o c.idx c.cmt

  $ ocaml-index aggregate -o ab.idx a.idx b.idx
  $ ocaml-index aggregate --standalone -o ab-standalone.idx a.idx b.idx

  $ ocaml-index aggregate -o non-standalone.idx ab.idx c.idx
  $ ocaml-index aggregate -o non-standalone-ab-standalone.idx ab-standalone.idx c.idx
  $ ocaml-index aggregate --standalone -o standalone.idx ab.idx c.idx

The standalone and non-standalone indexes contain the same data:

  $ ocaml-index dump non-standalone.idx > non-standalone.dump
  $ ocaml-index dump non-standalone-ab-standalone.idx > non-standalone-ab-standalone.dump
  $ ocaml-index dump standalone.idx > standalone.dump
  $ diff non-standalone.dump standalone.dump
  $ diff non-standalone-ab-standalone.dump standalone.dump

The non-standalone indexes are smaller:

  $ size () { wc -c < "$1"; }
  $ [ $(size non-standalone.idx) -lt $(size standalone.idx) ] && echo smaller
  smaller
  $ [ $(size non-standalone-ab-standalone.idx) -lt $(size standalone.idx) ] && echo smaller
  smaller

Create a function that temporarily removes an index file, which we'll use to
test the behavior of indexes when some are missing.

  $ with_removed () {
  >   file="$1"
  >   shift
  >   [ "$1" = -- ] && shift
  >   mv -- "$file" "$file.moved" || return
  >   "$@"
  >   status="$?"
  >   mv -- "$file.moved" "$file"
  >   return $status
  > }

The standalone index is the same as before, even when the component indexes are
missing.
  $ with_removed ab.idx -- ocaml-index dump standalone.idx | diff standalone.dump -
  $ with_removed a.idx -- ocaml-index dump standalone.idx | diff standalone.dump -
  $ with_removed b.idx -- ocaml-index dump standalone.idx | diff standalone.dump -
  $ with_removed c.idx -- ocaml-index dump standalone.idx | diff standalone.dump -

The non-standalone index raises an error if any component indexes are missing.
  $ with_removed ab.idx -- ocaml-index dump non-standalone.idx
  Missing file "$TESTCASE_ROOT/ab.idx".
  Hint: try to rebuild indexes with dune build @ocaml-index.
  5 uids:
  {uid: Foo; locs: 
  [1]
  $ with_removed a.idx -- ocaml-index dump non-standalone.idx
  Missing file "$TESTCASE_ROOT/a.idx".
  Hint: try to rebuild indexes with dune build @ocaml-index.
  5 uids:
  {uid: Foo; locs: 
  [1]
  $ with_removed b.idx -- ocaml-index dump non-standalone.idx
  Missing file "$TESTCASE_ROOT/b.idx".
  Hint: try to rebuild indexes with dune build @ocaml-index.
  5 uids:
  {uid: Foo; locs: "Foo": File "a.ml", line 1, characters 10-13;
                   
  [1]
  $ with_removed c.idx -- ocaml-index dump non-standalone.idx
  5 uids:
  {uid: Foo; locs:
     "Foo": File "a.ml", line 1, characters 10-13;
     "Foo": File "b.ml", line 1, characters 10-13;Missing file "$TESTCASE_ROOT/c.idx".
  Hint: try to rebuild indexes with dune build @ocaml-index.
  
     
  [1]

The non-standalone-ab-standalone index raises an error if the AB or C index is
missing, but not if A or B is missing..
  $ with_removed ab-standalone.idx -- ocaml-index dump non-standalone-ab-standalone.idx
  Missing file "$TESTCASE_ROOT/ab-standalone.idx".
  Hint: try to rebuild indexes with dune build @ocaml-index.
  5 uids:
  {uid: Foo; locs: 
  [1]
  $ with_removed a.idx -- ocaml-index dump non-standalone-ab-standalone.idx | diff non-standalone-ab-standalone.dump -
  $ with_removed b.idx -- ocaml-index dump non-standalone-ab-standalone.idx | diff non-standalone-ab-standalone.dump -
  $ with_removed c.idx -- ocaml-index dump non-standalone-ab-standalone.idx
  5 uids:
  {uid: Foo; locs:
     "Foo": File "a.ml", line 1, characters 10-13;
     "Foo": File "b.ml", line 1, characters 10-13;Missing file "$TESTCASE_ROOT/c.idx".
  Hint: try to rebuild indexes with dune build @ocaml-index.
  
     
  [1]
