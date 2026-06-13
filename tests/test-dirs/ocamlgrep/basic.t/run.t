Build the project so that cmt files are produced.

  $ $DUNE build @check 2>&1

Find every call to [List.length]. The jq filter renders the JSON
findings in the standalone ocamlgrep binary's text format: a
[file:start-end:] header followed by [N | source-line] gutter lines.
The header makes it unambiguous where one finding ends and the next
begins, even without a separator.

  $ $MERLIN single ocamlgrep -query 'List.length __' < /dev/null \
  >   | jq -r '
  >       .value.findings[]?
  >       | (if .start.line == .end.line
  >          then "\(.file):\(.start.line):\(.start.col)-\(.end.col):"
  >          else "\(.file):\(.start.line):\(.start.col)-\(.end.line):\(.end.col):"
  >          end),
  >         (range(0; .lines | length) as $i
  >          | "\(.start.line + $i) | \(.lines[$i])")
  >     '
  main.ml:3:10-24:
  3 |   let n = List.length xs in
  main.ml:4:10-34:
  4 |   let m = List.length [ "a"; "b" ] in
