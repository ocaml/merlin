  $ cat >main.ml <<EOF
  > let f = String.equal
  > module B : sig 
  >   val g : string -> string -> bool
  > end = struct
  >   module C = struct
  >     include External
  >     let g = equal
  >   end
  >   let g = C.g
  > end
  > EOF

  $ cat >external.ml <<EOF
  > let equal = String.equal
  > EOF

  $ ocamlc -bin-annot -bin-annot-occurrences -c external.ml main.ml

  $ ocaml-index aggregate -o main.uideps main.cmt

  $ ocaml-index aggregate -o test.uideps main.uideps

  $ ocaml-index dump main.uideps
  9 uids:
  {uid: External; locs: "External": File "main.ml", line 6, characters 12-20
   uid: External.0; locs: "equal": File "main.ml", line 7, characters 12-17
   uid: Main.0; locs: "f": File "main.ml", line 1, characters 4-5
   uid: Main.1; locs:
     "g": File "main.ml", line 7, characters 8-9;
     "C.g": File "main.ml", line 9, characters 10-13
   uid: Main.2; locs: "C": File "main.ml", line 5, characters 9-10
   uid: Main.3; locs: "g": File "main.ml", line 9, characters 6-7
   uid: Main.4; locs: "g": File "main.ml", line 3, characters 6-7
   uid: Main.5; locs: "B": File "main.ml", line 2, characters 7-8
   uid: Stdlib__String.173; locs:
     "String.equal": File "main.ml", line 1, characters 8-20
   }, 0 approx shapes: {}, and shapes for CUS .


  $ ocaml-index dump test.uideps
  9 uids:
  {uid: External; locs: "External": File "main.ml", line 6, characters 12-20
   uid: External.0; locs: "equal": File "main.ml", line 7, characters 12-17
   uid: Main.0; locs: "f": File "main.ml", line 1, characters 4-5
   uid: Main.1; locs:
     "g": File "main.ml", line 7, characters 8-9;
     "C.g": File "main.ml", line 9, characters 10-13
   uid: Main.2; locs: "C": File "main.ml", line 5, characters 9-10
   uid: Main.3; locs: "g": File "main.ml", line 9, characters 6-7
   uid: Main.4; locs: "g": File "main.ml", line 3, characters 6-7
   uid: Main.5; locs: "B": File "main.ml", line 2, characters 7-8
   uid: Stdlib__String.173; locs:
     "String.equal": File "main.ml", line 1, characters 8-20
   }, 0 approx shapes: {}, and shapes for CUS .

