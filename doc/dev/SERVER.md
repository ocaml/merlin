Merlin now implements a server. This simplify implementation of editor modes by
allowing synchronous process executions.

The `ocamlmerlin` binary is a wrapper, written in C, that redirects queries to
`ocamlmerlin-server`.

It can be used in a few different ways.

`old-protocol` works as a repl: one writes a query (formatted as a json value)
and reads an answer (also json). It is the protocol of merlin 1.x and 2.x.
When detecting old-protocol, `ocamlmerlin` wrapper simply executes the
ocamlmerlin-server. It is documented in [OLD-PROTOCOL.md](OLD-PROTOCOL.md).

With the new protocol, the query is specified on the command-line and the
content is read from standard input.  Merlin can now be used like a regular
UNIX command. Answers are written on standard output as JSON-values (or
optionally, S-expression).

In `single` mode, the wrapper executes `ocamlmerlin-server` and processes a
single query.
In `server` mode, the wrapper looks for an existing server. If none are found,
it executes a new one. Then it redirects the query to the server, wait for an
answer and terminates.

The editor plugin does the same work in both cases, caching & calling the
server is transparent.

Mode is specified as the first argument to `ocamlmerlin` binary, and defaults to
`old-protocol` for compatibility with previous versions.
