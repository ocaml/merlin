(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Positions
open Stretch
open Syntax

type early_producer =
  Positions.t *
  identifier located option *
  parameter *
  attributes

type early_producers =
  early_producer list

type early_production =
  early_producers *
  string located option * (* optional precedence *)
  branch_production_level *
  Positions.t

type early_productions =
  early_production list

let new_precedence_level =
  let c = ref 0 in
  fun (pos1, pos2) ->
    incr c;
    PrecedenceLevel (InputFile.get_input_file (), !c, pos1, pos2)

let new_production_level =
  let c = ref 0 in
  fun () ->
    incr c;
    ProductionLevel (InputFile.get_input_file (), !c)

let new_on_error_reduce_level =
  new_production_level
    (* the counter is shared with [new_production_level],
       but this is irrelevant *)

module IdSet = Set.Make (struct
  type t = identifier located
  let compare id1 id2 =
    compare (value id1) (value id2)
end)

let defined_identifiers (_, ido, _, _) accu =
  Option.fold IdSet.add ido accu

let defined_identifiers (producers : early_producers) =
  List.fold_right defined_identifiers producers IdSet.empty

let check_production_group (right_hand_sides : early_productions) =
  match right_hand_sides with
  | [] ->
      (* A production group cannot be empty. *)
      assert false
  | (producers, _, _, _) :: right_hand_sides ->
      let ids = defined_identifiers producers in
      List.iter (fun (producers, _, _, _) ->
        let ids' = defined_identifiers producers in
        try
          let id =
            IdSet.choose (IdSet.union
                                (IdSet.diff ids ids')
                                (IdSet.diff ids' ids))
          in
          Error.error [Positions.position id]
            "two productions that share a semantic action must define exactly\n\
             the same identifiers. Here, \"%s\" is defined\n\
             in one production, but not in all of them."
            (Positions.value id)
        with Not_found ->
          ()
      ) right_hand_sides

(* [normalize_producer i p] assigns a name of the form [_i]
   to the unnamed producer [p]. *)
let normalize_producer i (pos, opt_identifier, parameter, attrs) =
  let id =
    match opt_identifier with
      | Some id -> id
      | None -> Positions.with_pos pos ("_" ^ string_of_int (i + 1))
  in
  (id, parameter, attrs)

let normalize_producers (producers : early_producers) : producer list =
  List.mapi normalize_producer producers

let override pos o1 o2 =
  match o1, o2 with
  | Some _, Some _ ->
      Error.error [ pos ] "this production carries two %%prec declarations."
  | None, Some _ ->
      o2
  | _, None ->
      o1

(* Only unnamed producers can be referred to using positional identifiers.
   Besides, such positions must be taken in the interval [1
   .. List.length producers]. The output array [p] is such that
   [p.(idx) = Some x] if [idx] must be referred to using [x], not
   [$(idx + 1)]. *)
let producer_names (producers : early_producers) =
  producers
  |> List.map (fun (_, oid, _, _) -> Option.map Positions.value oid)
  |> Array.of_list

(* Check that a stretch contains an OCaml lowercase or uppercase identifier,
   and convert this stretch to a string. The stretch may be empty, too. *)

let validate_pointfree_action (ty : ocamltype) : Stretch.t option =
  match ty with
  | Inferred _ ->
      assert false
  | Declared stretch ->
      let s = stretch.stretch_raw_content in
      if Lexpointfree.validate_pointfree_action (Lexing.from_string s) then
        Some stretch
      else
        None

(* Test whether a string is a valid OCaml lowercase identifier. *)

(* [x] should be a LID, UID, or QID, produced by Menhir's main lexer.
   Testing its first character should suffice, but we are more cautious
   and validate it thoroughly. *)

let valid_ocaml_identifier (x : identifier located) : bool =
  Lexpointfree.valid_ocaml_identifier (Lexing.from_string (Positions.value x))
