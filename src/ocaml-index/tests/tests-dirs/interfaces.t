  $ cat >main.mli <<'EOF'
  > type t = Float.t
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c main.mli

  $ ls
  main.cmi
  main.cmti
  main.mli

  $ ocamlobjinfo -quiet -index main.cmti
  Indexed shapes:
  Unresolved: CU Stdlib . "Float"[module] . "t"[type]  :
    Float.t (File "main.mli", line 1, characters 9-16)

  $ ocaml-index aggregate main.cmti -o main.index

  $ ocaml-index dump main.index
  1 uids:
  {uid: Stdlib__Float.81; locs:
     "Float.t": File "main.mli", line 1, characters 9-16
   }, 0 approx shapes: {}, and shapes for CUS .
