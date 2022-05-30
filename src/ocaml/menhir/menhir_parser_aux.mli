(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module provides utilities that are shared by the two versions
   of the parser. *)

open Stretch
open Syntax

(* A few types used in the parser. *)

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

(* [new_precedence_level (pos1, pos2)] creates a new precendence level, which is
   stronger than any levels previously created by this function. It should be
   called every time a [%left], [%right], or [%nonassoc] declaration is found.
   The positions are the positions of this declaration in the source code. The
   precedence levels created by this function are attached to tokens and (via
   %prec) to productions. They are used in solving shift/reduce and
   shift/reduce/reduce conflicts. *)

val new_precedence_level: Lexing.position * Lexing.position -> precedence_level

(* [new_production_level()] creates a new production level, which is stronger
   than any levels previously created by this function. It should be called
   every time a new production is found. The production levels created by this
   function are attached to productions. They are used in solving
   reduce/reduce conflicts: following ocamlyacc and bison, the production that
   appears first in the grammar receives preference. It may seem very strange
   that %prec annotations do not influence this process, but that's how it is,
   at least for the moment. *)

val new_production_level: unit -> branch_production_level

(* [new_on_error_reduce_level()] creates a new level, which is attached to an
   [%on_error_reduce] declaration. *)

val new_on_error_reduce_level: unit -> on_error_reduce_level

(* [check_production_group] accepts a production group and checks that all
   productions in the group define the same set of identifiers. *)

val check_production_group: early_productions -> unit

(* [normalize_producers] accepts a list of producers where identifiers are
   optional and returns a list of producers where identifiers are mandatory.
   A missing identifier in the [i]-th position receives the conventional
   name [_i]. *)

val normalize_producers: early_producers -> producer list

(* [override pos oprec1 oprec2] decides which of the two optional
   %prec declarations [oprec1] and [oprec2] applies to a
   production. It signals an error if the two are present. *)

val override: Positions.t -> 'a option -> 'a option -> 'a option

(* [producer_names producers] returns an array [names] such that
   [names.(idx) = None] if the (idx + 1)-th producer is unnamed
   and [names.(idx) = Some id] if it is called [id]. *)

val producer_names: early_producers -> identifier option array

(* Check that a stretch represents valid content for a point-free semantic
   action, i.e., either just whitespace, or an OCaml lowercase or uppercase
   identifier. May raise [Lexpointfree.InvalidPointFreeAction]. *)

val validate_pointfree_action: ocamltype -> Stretch.t option

(* Test whether a string is a valid OCaml lowercase identifier. *)

val valid_ocaml_identifier: identifier located -> bool
