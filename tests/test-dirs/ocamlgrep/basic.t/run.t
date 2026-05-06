Build the project so that cmt files are produced.

  $ $DUNE build @check 2>&1

Find every call to [List.length]. The two-step pipeline strips the
JSON envelope down to the original ocamlgrep text format
[file:line:context] so the snapshot stays readable.

  $ $MERLIN single ocamlgrep -query 'List.length __' < /dev/null \
  >   | jq -r '.value.findings[]? | "\(.file):\(.line):\(.context)"'
  main.ml:3:  let n = List.length xs in
  main.ml:4:  let m = List.length [ "a"; "b" ] in
