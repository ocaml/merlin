  $ export BUILD_PATH_PREFIX_MAP="TEST_OCAML=$(dirname $(dirname $(command -v ocamlc))):$BUILD_PATH_PREFIX_MAP"
  $ cat >base__.ml <<'EOF'
  > module Or_error = Base__Or_error
  > module Not_built = Base__Not_built
  > EOF

  $ cat >or_error.ml <<'EOF'
  > type 'a t = ('a,'a) Result.t
  > let ok (x : 'a) : 'a t = Ok x
  > module Id (X : sig end ) = X
  > EOF

  $ cat >test.ml <<'EOF'
  > let x = Or_error.ok 42
  > EOF

  $ $OCAMLC -c -o Base__Or_error or_error.ml

  $ $OCAMLC -c -no-alias-deps base__.ml
  File "base__.ml", line 2, characters 19-34:
  2 | module Not_built = Base__Not_built
                         ^^^^^^^^^^^^^^^
  Warning 49 [no-cmi-file]: no cmi file was found
    in path for module Base__Not_built

  $ $OCAMLC -c -open Base__ test.ml

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

  $ cat >.merlin <<'EOF'
  > FLG -short-paths -open Base__
  > EOF

  $ $MERLIN single type-enclosing -log-file log -position 1:4 \
  > -filename test.ml < test.ml | jq '.value[0].type'
  "int Or_error.t"

We should never try to load the cmi for Not_build
  $ cat log | grep reading 
  reading "$TESTCASE_ROOT" from disk
  reading "TEST_OCAML/lib/ocaml" from disk
  reading "TEST_OCAML/lib/ocaml/stdlib.cmi" from disk
  reading "$TESTCASE_ROOT/base__.cmi" from disk
  reading "$TESTCASE_ROOT/Base__Or_error.cmi" from disk
  reading "TEST_OCAML/lib/ocaml/stdlib__Result.cmi" from disk
  reading "TEST_OCAML/lib/ocaml/camlinternalFormatBasics.cmi" from disk
