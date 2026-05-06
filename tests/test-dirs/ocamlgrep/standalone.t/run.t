Smoke test for the standalone [ocamlgrep] binary. The output is the
original ocamlgrep CLI format ([file:line:context]); colors are
disabled via the standard NO_COLOR environment variable.

  $ $DUNE build @check 2>&1

  $ NO_COLOR=1 ocamlgrep 'List.length __'
  main.ml:3:  let _ = List.length xs in
  main.ml:4:  let _ = List.length [ 4; 5 ] in
