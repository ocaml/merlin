Reproduce [ocaml-lsp #344](https://github.com/ocaml/ocaml-lsp/issues/344): a symbol's
documentation is some other random documentation from a library that was last pulled for
completion. See the issue for details. The space is necessary for the position of
documentation to be fetch-able.

  $ cat >lib_doc.ml <<EOF
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > let k = ()
  > let m = List.map
  > EOF

we need merlin to keep state between requests, so using server

  $ $MERLIN server stop-server

see that there is no doc for k

  $ $MERLIN server document -position 23:5 < lib_doc.ml | jq '.value'
  "No documentation available"

we trigger the bug

  $ $MERLIN server document -position 24:15 -filename lib_doc < lib_doc.ml
  {
    "class": "return",
    "value": "[map f [a1; ...; an]] applies function [f] to [a1, ..., an],
     and builds the list [[f a1; ...; f an]]
     with the results returned by [f].",
    "notifications": []
  }

random documentation is fetched for the same `document` request as before

  $ $MERLIN server document -position 23:5 < lib_doc.ml
  {
    "class": "return",
    "value": "No documentation available",
    "notifications": []
  }

stop server

  $ $MERLIN server stop-server
