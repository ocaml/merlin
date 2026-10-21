This test check the robustness and flexibility of the -o argument

  $ cat >a.ml <<'EOF2'
  > let x = 1
  > let y = x + 1
  > EOF2
  $ cat >b.ml <<'EOF2'
  > let z = A.x + A.y
  > EOF2
  $ ocamlc -bin-annot -bin-annot-occurrences -c a.ml b.ml
  $ ocaml-index aggregate a.cmt -o a.ocaml-index
  $ ocaml-index aggregate b.cmt -o b.ocaml-index
  $ ocaml-index stats a.ocaml-index b.ocaml-index | grep -v "^- [0a-z]"
  Index "b.ocaml-index" contains:
  - 5 definitions
  - 6 locations
  
  Index "a.ocaml-index" contains:
  - 3 definitions
  - 4 locations
  

A plain relative output path, read from the current directory and from
elsewhere:

  $ mkdir out elsewhere
  $ ocaml-index aggregate a.ocaml-index b.ocaml-index -o out/plain.ocaml-index
  $ ocaml-index stats out/plain.ocaml-index
  Index "out/plain.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
  $ (cd elsewhere && ocaml-index stats ../out/plain.ocaml-index)
  Index "../out/plain.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  

An absolute output path:

  $ ocaml-index aggregate a.ocaml-index b.ocaml-index -o $PWD/out/absolute.ocaml-index
  $ ocaml-index stats out/absolute.ocaml-index
  Index "out/absolute.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
  $ (cd elsewhere && ocaml-index stats ../out/absolute.ocaml-index)
  Index "../out/absolute.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  

An output path starting with `./`:

  $ ocaml-index aggregate a.ocaml-index b.ocaml-index -o ./out/dot.ocaml-index
  $ ocaml-index stats out/dot.ocaml-index
  Index "out/dot.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
  $ (cd elsewhere && ocaml-index stats ../out/dot.ocaml-index)
  Index "../out/dot.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  

Aggregating from a subdirectory into its parent, with `..` in the output path:

  $ cp a.ocaml-index b.ocaml-index out/
  $ (cd out && ocaml-index aggregate a.ocaml-index b.ocaml-index -o ../dotdot.ocaml-index)
  $ ocaml-index stats dotdot.ocaml-index
  Index "dotdot.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
  $ (cd elsewhere && ocaml-index stats ../dotdot.ocaml-index)
  Index "../dotdot.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  

Inputs given with `..` in their path:

  $ (cd elsewhere && ocaml-index aggregate ../a.ocaml-index ../b.ocaml-index -o ../out/dotdot-inputs.ocaml-index)
  $ ocaml-index stats out/dotdot-inputs.ocaml-index
  Index "out/dotdot-inputs.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
  $ (cd elsewhere && ocaml-index stats ../out/dotdot-inputs.ocaml-index)
  Index "../out/dotdot-inputs.ocaml-index" contains:
  - 5 definitions
  - 10 locations
  - 0 approximated definitions
  - 0 compilation units shapes
  - root dir: none
  
