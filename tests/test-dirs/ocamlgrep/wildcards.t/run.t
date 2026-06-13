Build the project so that cmt files are produced.

  $ $DUNE build @check 2>&1

The plain wildcard [__] matches any expression. Numbered wildcards
[__1], [__2], ... must match the *same* expression every time they
appear. So [Some __1 -> Some __1] only matches the identity branch.

This pattern matches a multi-line [match] expression, exercising
Format A's multi-line header.

  $ $MERLIN single ocamlgrep \
  >   -query 'match __ with None -> __ | Some __1 -> Some __1' \
  >   < /dev/null \
  >   | jq -r '
  >       .value.findings[]?
  >       | (if .start.line == .end.line
  >          then "\(.file):\(.start.line):\(.start.col)-\(.end.col):"
  >          else "\(.file):\(.start.line):\(.start.col)-\(.end.line):\(.end.col):"
  >          end),
  >         (range(0; .lines | length) as $i
  >          | "\(.start.line + $i) | \(.lines[$i])")
  >     '
  main.ml:3:2-5:20:
  3 |   match x with
  4 |   | None -> None
  5 |   | Some y -> Some y
