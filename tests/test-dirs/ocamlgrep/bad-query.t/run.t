Build the project so that cmt files are produced.

  $ $DUNE build @check 2>&1

A query that does not parse as an OCaml expression is reported as a
[failure] in the response envelope rather than crashing the server.

  $ $MERLIN single ocamlgrep -query 'let x =' < /dev/null \
  >   | jq -r '"\(.class): \(.value)"'
  failure: Could not parse search expression.

A missing query argument is also a failure (the [-query] flag is
mandatory).

  $ $MERLIN single ocamlgrep < /dev/null \
  >   | jq -r '"\(.class): \(.value)"'
  failure: -query <pattern> is mandatory
