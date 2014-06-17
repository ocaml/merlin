- Replace Browse by a propper wrapping of Typedtree
- Find proper API for incremental parser
  -> goto table should not be manipulated explicitly
  -> exceptions from semantic action should be caught and treated differently
  -> relying on special handling of stack bottom is not good either
- perf idea: send batches of token rather than entering/exiting the whole
  parser barrier for each token
- backport support for ocaml 4.00 & 4.01
- features to port:
  -> locate
- features to test:
  -> completion
  -> type enclosing / type expr
  -> occurences
- catch "different assumptions" exception
- recovery heuristic is really not sufficient, a few test cases easily trigger
  bad recursion:
  let f =
    let bad
    let x = () in
- tell marker: after tell, seeking to the first state without marker
  might be an easy way to get slightly faster on thousand lines files
