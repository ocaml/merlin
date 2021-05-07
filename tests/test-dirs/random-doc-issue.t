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
    "value": " [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
     and builds the list [[f a1; ...; f an]]
     with the results returned by [f]. Not tail-recursive.
   ",
    "notifications": []
  }

random documentation is fetched for the same `document` request as before

  $ $MERLIN server document -position 23:5 < lib_doc.ml
  {
    "class": "return",
    "value": "List operations.
  
     Some functions are flagged as not tail-recursive.  A tail-recursive
     function uses constant stack space, while a non-tail-recursive function
     uses stack space proportional to the length of its list argument, which
     can be a problem with very long lists.  When the function takes several
     list arguments, an approximate formula giving stack usage (in some
     unspecified constant unit) is shown in parentheses.
  
     The above considerations can usually be ignored if your lists are not
     longer than about 10000 elements.
  
     The labeled version of this module can be used as described in the
     {!StdLabels} module.",
    "notifications": []
  }

stop server

  $ $MERLIN server stop-server
