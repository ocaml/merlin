/**
* VARIANTS
**/

  $ cat >constr.ml <<EOF
  > module C = struct type t = A of int |  B end
  > let foo : C.t = C.A 42
  > EOF

We expect 1:27
  $ $MERLIN single locate -look-for mli -position 2:18 \
  > -filename ./constr.ml < ./constr.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/constr.ml",
    "pos": {
      "line": 1,
      "col": 27
    }
  }

FIXME: this is not a very satisfying answer. 
We expect 1:18
  $ $MERLIN single locate  -look-for ml -position 2:12 \
  > -filename ./constr.ml < ./constr.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/constr.ml",
    "pos": {
      "line": 1,
      "col": 18
    }
  }

With the declaration in another compilation unit:
  $ cat >other_module.ml <<EOF
  > let foo = Constr.C.B
  > EOF

  $ $OCAMLC -c -bin-annot constr.ml

  $ $MERLIN single locate -look-for mli -position 1:19 \
  > -filename ./other_module.ml < ./other_module.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/constr.ml",
    "pos": {
      "line": 1,
      "col": 36
    }
  }

/**
* POLYMORPHIC VARIANTS
**/
  $ cat >constr.ml <<EOF
  > type t = [\`A of int | \`B]
  > let foo : t = \`A 42
  > EOF

FIXME: we could expect constr.ml 1:11
  $ $MERLIN single locate -look-for mli -position 2:14 \
  > -filename ./constr.ml < ./constr.ml | jq '.value'
  "Not a valid identifier"

FIXME: we could expect constr.ml 1:11
  $ $MERLIN single locate -look-for ml -position 2:14 \
  > -filename ./constr.ml < ./constr.ml | jq '.value'
  "Not a valid identifier"

With the declaration in another compilation unit:
  $ cat >other_module.ml <<EOF
  > let foo = Constr.\`B
  > EOF

  $ $OCAMLC -c -bin-annot constr.ml

FIXME: we expect constr.ml 1:23
  $ $MERLIN single locate -look-for mli -position 1:18 \
  > -filename ./other_module.ml < ./other_module.ml | jq '.value'
  "Not a valid identifier"
