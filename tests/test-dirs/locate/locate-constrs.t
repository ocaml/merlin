/**
* VARIANTS
**/

  $ cat >constr.ml <<EOF
  > type t = A of int |  B
  > let foo : t = A 42
  > EOF

  $ $MERLIN single locate -look-for mli -position 2:14 \
  > -filename ./constr.ml < ./constr.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/constr.ml",
    "pos": {
      "line": 1,
      "col": 9
    }
  }

  $ $MERLIN single locate  -look-for ml -position 2:14 \
  > -filename ./constr.ml < ./constr.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/constr.ml",
    "pos": {
      "line": 1,
      "col": 9
    }
  }

With the declaration in another compilation unit:
  $ cat >other_module.ml <<EOF
  > let foo = Constr.B
  > EOF

  $ $OCAMLC -c -bin-annot constr.ml

  $ $MERLIN single locate -look-for mli -position 1:17 \
  > -filename ./other_module.ml < ./other_module.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/constr.ml",
    "pos": {
      "line": 1,
      "col": 21
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
