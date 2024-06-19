  $ cat >test.ml <<'EOF'
  > let f x ~lbl_a y ~lbl_b z = ignore (x, lbl_a, lbl_b, z)
  > let _ = f
  > EOF

First is the first parameter:
  $ $MERLIN single signature-help -position 2:11 -filename test <test.ml |
  > jq '.value.activeParameter'
  0

  $ cat >test.ml <<'EOF'
  > let f x ~lbl_a y ~lbl_b z = ignore (x, lbl_a, lbl_b, z)
  > let _ = f 0 
  > EOF

After we expect the next non-labelled parameter to be active:
  $ $MERLIN single signature-help -position 2:12 -filename test <test.ml |
  > jq '.value.activeParameter'
  2

  $ cat >test.ml <<'EOF'
  > let f x ~lbl_a y ~lbl_b z = ignore (x, lbl_a, lbl_b, z)
  > let _ = f 0 3
  > EOF

It does happen when the expression is being written:
  $ $MERLIN single signature-help -position 2:12 -filename test <test.ml |
  > jq '.value.activeParameter'
  2

  $ cat >test.ml <<'EOF'
  > let f x ~lbl_a y ~lbl_b z = ignore (x, lbl_a, lbl_b, z)
  > let _ = f 0 ~
  > EOF

And when a tilde is used the first labelled arg is higlighted:
  $ $MERLIN single signature-help -position 2:13 -filename test <test.ml |
  > jq '.value.activeParameter'
  1

  $ cat >test.ml <<'EOF'
  > let f x ~lbl_a y ~lbl_b z = ignore (x, lbl_a, lbl_b, z)
  > let _ = f 0 ~lbl_b
  > EOF

Or the second one is the name corresponds:
  $ $MERLIN single signature-help -position 2:18 -filename test <test.ml |
  > jq '.value.activeParameter'
  3

If we write some positional arguments first they should not be suggested later:
  $ cat >test.ml <<'EOF'
  > let f x ~lbl_a y ~lbl_b z = ignore (x, lbl_a, lbl_b, z, y);;
  > let _ = f 1 3 ~lbl_a:4 
  > EOF

  $ $MERLIN single signature-help -position 2:23 -filename test <test.ml |
  > jq '.value.activeParameter'
  4
