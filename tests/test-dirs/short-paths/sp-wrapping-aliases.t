
  $ cat >base__.ml <<'EOF'
  > module Or_error = Base__Or_error
  > EOF

  $ cat >or_error.ml <<'EOF'
  > type 'a t = ('a,'a) Result.t
  > let ok (x : 'a) : 'a t = Ok x
  > module Id (X : sig end ) = X
  > EOF

  $ cat >test.ml <<'EOF'
  > let x = Or_error.ok 42
  > EOF

  $ cat >test_f.ml <<'EOF'
  > module _ = Or_error.Id (struct end)
  > EOF

  $ cat >test2.ml <<'EOF'
  > let x = Test.x (* No mention of Or_error at all *)
  > EOF

  $ $OCAMLC -c -o Base__Or_error or_error.ml

  $ $OCAMLC -c base__.ml

  $ $OCAMLC -c -open Base__ test.ml

  $ $OCAMLC -c -open Base__ test2.ml

  $ $OCAMLC -c -open Base__ test_f.ml

  $ ls
  Base__Or_error.cmi
  Base__Or_error.cmo
  base__.cmi
  base__.cmo
  base__.ml
  or_error.ml
  test.cmi
  test.cmo
  test.ml
  test2.cmi
  test2.cmo
  test2.ml
  test_f.cmi
  test_f.cmo
  test_f.ml

  $ cat >.merlin <<'EOF'
  > FLG -short-paths -open Base__
  > EOF

  $ $MERLIN single type-enclosing -log-file log -log-section short-paths -position 1:4 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "int Or_error.t"

  $ $MERLIN single type-enclosing -index 0 -position 1:11 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "(module Or_error)"

  $ $MERLIN single type-enclosing -index 0 \
  > -log-file log  -position 1:22 \
  > -filename test_f.ml < test_f.ml | jq '.value[0].type'
  "(module Or_error.Id)"

It works even if Or_error is not used
  $ $MERLIN single type-enclosing -index 0 -position 1:4 \
  > -filename test2.ml < test2.ml | jq '.value[0].type'
  "int Or_error.t"
