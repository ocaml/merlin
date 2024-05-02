FIXME: locate on `M.(|+)` should work:
  $ $MERLIN single locate -position 2:11 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let (+) a b = a + b end
  > let _ = M.(+)
  > EOF
  "Not in environment 'M.+'"

Locate on `M.(+|)` should work:
  $ $MERLIN single locate -position 2:12 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let (+) a b = a + b end
  > let _ = M.(+)
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 1,
      "col": 22
    }
  }

And need spaces:
FIXME: locate on `M.(| * )` should work:
  $ $MERLIN single locate -position 2:11 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let ( * ) a b = a + b end
  > let _ = M.( * )
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

And need spaces:
FIXME: locate on `M.( |* )` should work:
  $ $MERLIN single locate -position 2:12 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let ( * ) a b = a + b end
  > let _ = M.( * )
  > EOF
  "Comment not terminated"

And need spaces:
FIXME: locate on `M.( *| )` should work:
  $ $MERLIN single locate -position 2:13 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let ( * ) a b = a + b end
  > let _ = M.( * )
  > EOF
  "Comment not terminated"
