# Small test

A file with 2 type errors:
- arg passed to "maybe_gen" should be unit, it is an int
- return type of "maybe_gen" is int option, in a context where an int is expected

  $ cat >foo.ml <<EOF
  > let maybe_gen () = Some 0 in
  > (maybe_gen 3) + 2
  > EOF

  $ $MERLIN single errors -filename foo.ml < foo.ml | grep "message" | wc -l
  2

The number of "saved parts" corresponds to the number of errors:

  $ $MERLIN single dump -what browse -filename foo.ml < foo.ml | \
  > grep "saved-parts" | wc -l
  2


# Bigger test

Basically the same test with one level of nesting, we go from 2 to 4 errors:

  $ cat >foo.ml <<EOF
  > let maybe_gen () = Some 0 in
  > (maybe_gen ((maybe_gen 3) + 2)) + 3
  > EOF

  $ $MERLIN single errors -filename foo.ml < foo.ml | grep "message" | wc -l
  4

And the number of saved parts stays in sync with the number of errors:

  $ $MERLIN single dump -what browse -filename foo.ml < foo.ml | \
  > grep "saved-parts" | wc -l
  4

And we could nest once more, and everything is fine and linear:

  $ cat >foo.ml <<EOF
  > let maybe_gen () = Some 0 in
  > (maybe_gen ((maybe_gen ((maybe_gen 3) + 2)) + 3)) + 4
  > EOF

  $ $MERLIN single errors -filename foo.ml < foo.ml | grep "message" | wc -l
  6

  $ $MERLIN single dump -what browse -filename foo.ml < foo.ml | \
  > grep "saved-parts" | wc -l
  6

