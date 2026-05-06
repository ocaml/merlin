Build the project so that cmt files are produced.

  $ $DUNE build @check 2>&1

The plain wildcard [__] matches any expression. Numbered wildcards
[__1], [__2], ... must match the *same* expression every time they
appear. So [Some __1 -> Some __1] only matches the identity branch.

  $ $MERLIN single ocamlgrep \
  >   -query 'match __ with None -> __ | Some __1 -> Some __1' \
  >   < /dev/null \
  >   | jq -r '.value.findings[]? | "\(.file):\(.line):\(.context)"'
  main.ml:3:  match x with
