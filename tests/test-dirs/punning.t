# Testing queries on punned forms

  $ cat > let_punning.ml <<EOF
  > let (let+) x f = f (fst x)
  > 
  > let f (x : int * _) =
  >   let+ x in
  >   x
  > EOF

Should locate to the `x` in `f x`
  $ $MERLIN single locate -position 4:7 -filename let_punning.ml < let_punning.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/let_punning.ml",
    "pos": {
      "line": 3,
      "col": 7
    }
  }

FIXME: Should answer the type of x in the pattern: "int"
  $ $MERLIN single type-enclosing -position 4:7 -filename let_punning.ml < let_punning.ml | jq .value[0].type
  "int"

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
