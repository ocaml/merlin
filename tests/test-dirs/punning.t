# Testing locate on let punnings

  $ cat > let_punning.ml <<EOF
  > let (let+) x f = f x
  > 
  > let f x =
  >   let+ x in
  >   x
  > EOF

FIXME: Should locate to the `x` in `f x`, not say that we are at the definition
  $ $MERLIN single locate -position 4:7 -filename let_punning.ml < let_punning.ml | jq .value
  "Already at definition point"

# Testing locate on argument punnings

  $ cat > arg_punning.ml <<EOF
  > let f ~x = x
  > 
  > let g x = f ~x
  > EOF

  $ $MERLIN single locate -position 3:13 -filename arg_punning.ml < arg_punning.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/arg_punning.ml",
    "pos": {
      "line": 3,
      "col": 6
    }
  }

# Testing locate on field punnings

  $ cat > field_punning.ml <<EOF
  > type record = { x : int; y : int }
  > 
  > let make x y = {x; y}
  > EOF

  $ $MERLIN single locate -position 3:16 -filename field_punning.ml < field_punning.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/field_punning.ml",
    "pos": {
      "line": 3,
      "col": 9
    }
  }
