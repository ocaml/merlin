  $ cat >test.ml <<'EOF'
  > type t = {
  >   label_a : int
  > }
  > 
  > let v = { label_a = 42 }
  > let () = print_int v.label_a 
  > EOF

When cursor on the usage, all occurrences are highlighted
  $ $MERLIN single occurrences -identifier-at 6:25 -filename test.ml <test.ml | 
  > jq '.value[].start.line'
  2
  5
  6

When cursor on the definition, occurrences are not highlighted
  $ $MERLIN single occurrences -identifier-at 2:5 -filename test.ml <test.ml |
  > jq '.value[].start.line'
  2
  5
  6

Same test for constructors:
  $ cat >test.ml <<'EOF'
  > type t = Constr_a of int | No_param
  > 
  > let _ = Constr_a 42 
  > let _ = No_param 
  > EOF

When cursor on the usage, all occurrences are highlighted
  $ $MERLIN single occurrences -identifier-at 4:10 -filename test.ml <test.ml | 
  > jq '.value[].start.line'
  1
  4

When cursor on the definition, occurrences are not highlighted
  $ $MERLIN single occurrences -identifier-at 1:30 -filename test.ml <test.ml |
  > jq '.value[].start.line'
  1
  4

When cursor on the usage, all occurrences are highlighted
  $ $MERLIN single occurrences -identifier-at 3:10 -filename test.ml <test.ml | 
  > jq '.value[].start.line'
  1
  3

When cursor on the definition, occurrences are not highlighted
  $ $MERLIN single occurrences -identifier-at 1:10 -filename test.ml <test.ml |
  > jq '.value[].start.line'
  1
  3
