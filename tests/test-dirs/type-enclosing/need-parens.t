Locate on `M.(|+)` should work:
  $ $MERLIN single locate -position 2:11 -filename test.ml <<'EOF' | \
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
Locate on `M.(| * )` should work:
  $ $MERLIN single locate -position 2:11 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let ( * ) a b = a + b end
  > let _ = M.( * )
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 1,
      "col": 7
    }
  }

And need spaces:
Locate on `M.( |* )` should work:
  $ $MERLIN single locate -position 2:12 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let ( * ) a b = a + b end
  > let _ = M.( * )
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 1,
      "col": 22
    }
  }

And need spaces:
Locate on `M.( *| )` should work:
  $ $MERLIN single locate -position 2:13 -filename test.ml <<'EOF' | \
  > jq '.value'
  > module M = struct let ( * ) a b = a + b end
  > let _ = M.( * )
  > EOF
  {
    "file": "test.ml",
    "pos": {
      "line": 1,
      "col": 22
    }
  }
