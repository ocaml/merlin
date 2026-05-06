Smoke test for the standalone [ocamlgrep] binary in Format A: a
[file:start-end:] header followed by gutter source lines. Colors are
disabled via the standard NO_COLOR environment variable so the
snapshot is plain text.

  $ $DUNE build @check 2>&1

  $ NO_COLOR=1 ocamlgrep 'List.length __'
  main.ml:3:10-24:
  3 |   let _ = List.length xs in
  main.ml:4:10-30:
  4 |   let _ = List.length [ 4; 5 ] in
