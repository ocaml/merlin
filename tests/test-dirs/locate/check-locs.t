  $ cat >locs.ml <<EOF
  > module M : sig
  >   val x : int (* Psig_value *)
  >   external e : int = "ext" 
  >   type t = A of int (* Psig_type *)
  >   type te = ..
  >   type te += B (* Psig_typeext *)
  >   exception E (* Psig_exception *)
  >   module A : sig end (* Psig_module *)
  >   module rec R1 : sig end 
  >   and R2 : sig end (* Psig_recmodule *)
  >   module type S = sig end (* Psig_modtype *)
  >   class c : object end (* Psig_class *)
  >   and d : object end
  >   class type ct = object end (* Psig_class_type *)
  >   and dt = object end
  > end = struct
  >   let x = 4 (* Pstr_value *)
  >   external e : int = "ext" (* Pstr_primitive *)
  >   type t = A of int (* Pstr_type *)
  >   type te = ..
  >   type te += B (* Pstr_typeext *)
  >   exception E (* Pstr_exception *)
  >   module A = struct end (* Pstr_module *)
  >   module rec R1 : sig end = struct end 
  >   and R2 : sig end = struct end (* Pstr_recmodule *)
  >   module type S = sig end (* Pstr_modtype *)
  >   class c = object end (* Pstr_class *)
  >   and d = object end
  >   class type ct = object end (* Pstr_class_type *)
  >   and dt = object end
  > end
  > open M
  > let _ = x
  > let _ = e
  > let _ : t = A 42
  > let _ : te = B
  > let _  = raise E
  > module _ = A
  > module _ = R1
  > module _ = R2
  > module _ : S = A
  > let _ = new c 
  > let _ = new d
  > let _ = fun (_ : ct) (_ : dt) -> ()
  > EOF

FIXME Most of the following locs are wrong. 

We expect merlin to jump to the identifier, not the beginning of the definition
/ declaration.

(* value *)
  $ $MERLIN  single locate -look-for mli -position 33:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 2,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 33:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 17,
    "col": 6
  }

(* primitive *)
  $ $MERLIN  single locate -look-for mli -position 34:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 3,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 34:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 18,
    "col": 2
  }

(* type *)
  $ $MERLIN  single locate -look-for mli -position 35:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 4,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 35:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 19,
    "col": 2
  }

(* typext *)
  $ $MERLIN  single locate -look-for mli -position 36:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 5,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 36:8 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 20,
    "col": 2
  }

(* exception *)
  $ $MERLIN  single locate -look-for mli -position 37:15 \
  > -filename ./locs.ml < ./locs.ml  | jq ".value.pos"
  {
    "line": 7,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 37:15 \
  > -filename ./locs.ml < ./locs.ml  | jq ".value.pos"
  {
    "line": 22,
    "col": 2
  }

(* module *)
  $ $MERLIN  single locate -look-for mli -position 38:11 \
  > -filename ./locs.ml < ./locs.ml  | jq ".value.pos"
  {
    "line": 8,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 38:11 \
  > -filename ./locs.ml < ./locs.ml  | jq ".value.pos"
  {
    "line": 23,
    "col": 2
  }

(* module rec *)
  $ $MERLIN  single locate -look-for mli -position 39:11 \
  > -filename ./locs.ml < ./locs.ml  | jq ".value.pos"
  {
    "line": 9,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 39:11 \
  > -filename ./locs.ml < ./locs.ml  | jq ".value.pos"
  {
    "line": 9,
    "col": 2
  }

  $ $MERLIN  single locate -look-for mli -position 40:11 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 10,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 40:11 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 10,
    "col": 2
  }

(* module type *)
  $ $MERLIN  single locate -look-for mli -position 41:12 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 11,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 41:12 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 26,
    "col": 2
  }

(* class *)
  $ $MERLIN  single locate -look-for mli -position 42:12 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 12,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 42:12 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 27,
    "col": 8
  }

  $ $MERLIN  single locate -look-for mli -position 43:12 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 13,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 43:12 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 28,
    "col": 6
  }

(* class type *)
  $ $MERLIN  single locate -look-for mli -position 44:17 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 14,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 44:17 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 29,
    "col": 13
  }

  $ $MERLIN  single locate -look-for mli -position 44:26 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 15,
    "col": 2
  }

  $ $MERLIN  single locate -look-for ml -position 44:26 \
  > -filename ./locs.ml < ./locs.ml | jq ".value.pos"
  {
    "line": 30,
    "col": 6
  }
