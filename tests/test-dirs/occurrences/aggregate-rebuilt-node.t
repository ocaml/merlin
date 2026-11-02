  $ for i in $(seq 1 71); do touch m$i.ml; done
  $ $OCAMLC -c m*.ml

  $ for i in $(seq 1 71); do echo "module _ = M$i"; done > a.ml
  $ echo "let x = 0" > b.ml
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c a.ml b.ml

  $ ocaml-index aggregate -o a.index a.cmt
  $ ocaml-index aggregate -o b.index b.cmt

This used to crash with:
Fatal error: exception Invalid_argument("Granular_marshal.fetch_parent: Unexpected parent link On_disk_ptr(loc=55)")
  $ ocaml-index aggregate -o ab.index a.index b.index

Check the result is equivalent to direct indexing:

  $ ocaml-index aggregate -o direct.index a.cmt b.cmt
  $ ocaml-index stats ab.index direct.index
  Index "direct.index" contains:
  - 72 definitions
  - 72 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
  Index "ab.index" contains:
  - 72 definitions
  - 72 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
