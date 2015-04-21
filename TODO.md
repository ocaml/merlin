- Locate:
  -> associate ocamldoc comments to trie nodes when building the trie.

- Relegated to next menhir version:
  Find proper API for incremental parser
  -> goto table should not be manipulated explicitly
  -> exceptions from semantic action should be caught and treated differently
  -> relying on special handling of stack bottom is not good either
     Bottom handling code improved, but we should make sure it s stable.

- Test: (gooby pls)
  -> completion
  -> locate
  -> type enclosing / type expr
  -> occurrences

- Module constraint relaxation is wrong on functors argument. 
  Check how typer behaves with incorrect functors.
  Behavior of module error recovery is unclear.

IDEA (later)
- lexing: 
  composable lexers with a clean interface for resumption would make support
  for other languages / preprocessors easier
- syntax recovery (studious-parser branch):
  learn from suffix to target specific states during recovery
- error explanation:
  use machine-learning techniques to scan opam codebase, identify common
  patterns, suggest common solution in presence of an error
