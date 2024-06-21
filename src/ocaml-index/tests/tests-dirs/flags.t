  $ cat >main.ml <<EOF
  > let x = 42
  > let () = print_int x
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c main.ml

  $ ocaml-index aggregate main.cmt

Default output file:
  $ ls project.ocaml-index
  project.ocaml-index

Set output file:
  $ ocaml-index aggregate main.cmt -o out
  $ ls out
  out

No root dir was given:
  $ ocaml-index stats project.ocaml-index | grep root
  - root dir: none

We provide one:
  $ ocaml-index aggregate main.cmt --root /tmp/
  $ ocaml-index stats project.ocaml-index | grep root
  - root dir: /tmp/

Rewrite locations:
  $ ocaml-index aggregate main.cmt --root /tmp/ --rewrite-root
  $ ocaml-index dump project.ocaml-index | grep File
     "x": File "/tmp/main.ml", line 1, characters 4-5;
     "x": File "/tmp/main.ml", line 2, characters 19-20
     "print_int": File "/tmp/main.ml", line 2, characters 9-18
