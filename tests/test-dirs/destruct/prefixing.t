#################
### FROM EXPR ###
#################
Test 1.1
  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename typ.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct type my_list = Atom | Elt of string * my_list end
  > let f x : A.my_list =
  >   x
  > EOF
  "match (x : A.my_list) with | A.Atom -> _ | A.Elt (_, _) -> _"

Test 1.2
  $ $MERLIN single case-analysis -start 4:2 -end 4:3 -filename typ.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct type my_list = Atom | Elt of string * my_list end
  > open A
  > let f x : my_list =
  >   x
  > EOF
  "match (x : my_list) with | Atom -> _ | Elt (_, _) -> _"

Test 1.3
  $ $MERLIN single case-analysis -start 6:2 -end 6:3 -filename typ.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct type t = A | B | C end
  > module B = struct type t = A | B end
  > open A
  > open B
  > let f x : A.t =
  >   x
  > EOF
  "match (x : A.t) with | A -> _ | B -> _ | C -> _"

Test 1.4
  $ $MERLIN single case-analysis -start 5:2 -end 5:3 -filename typ.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct module B = struct type t = C end end
  > open A
  > module B = struct type t = D end
  > let f x : A.B.t =
  >   x
  > EOF
  "match (x : A.B.t) with | A.B.C -> _"

################
### COMPLETE ###
################

Test 2.1
  $ cat >typ21.ml <<EOF
  > module A = struct type t = B | C end
  > let f (x : A.t) =
  >   match x with
  >   | A.B -> ()
  > open A
  > let f (x : t) =
  >   match x with
  >   | B -> ()
  > EOF

Test 2.2
  $ $MERLIN single case-analysis -start 4:4 -end 4:4 -filename typ21.ml <typ21.ml | \
  > tr -d '\n' | jq '.value[1]'
  "| A.C -> _"

Test 2.3
  $ $MERLIN single case-analysis -start 8:4 -end 8:4 -filename typ21.ml <typ21.ml | \
  > tr -d '\n' | jq '.value[1]'
  "| C -> _"

Test 2.4
  $ $MERLIN single case-analysis -start 5:4 -end 5:4 -filename typ.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct module B = struct type t = C | F end end
  > open A
  > module B = struct type t = D end
  > let f x = match (x : A.B.t) with
  >   | A.B.C -> ()
  > EOF
  "| A.B.F -> _"

################
### REFINING ###
################

Test 3.1
  $ $MERLIN single case-analysis -start 5:9 -end 5:10 -filename refine_pattern.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct type t = B | C end
  > let _ =
  >   match (None : A.t option) with
  >   | None -> ()
  >   | Some _ -> ()
  > EOF
  "Some (A.B) | Some (A.C)"

Test 3.2
  $ $MERLIN single case-analysis -start 6:9 -end 6:10 -filename refine_pattern.ml <<EOF | \
  > tr -d '\n' | jq '.value[1]'
  > module A = struct type t = B | C end
  > open A
  > let _ =
  >   match (None : t option) with
  >   | None -> ()
  >   | Some _ -> ()
  > EOF
  "Some (B) | Some (C)"
