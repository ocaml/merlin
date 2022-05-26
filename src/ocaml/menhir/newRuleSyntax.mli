(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Menhir_syntax

(* The new rule syntax is desugared to the old rule syntax.
   The translation exploits anonymous rules, so it must be
   performed before anonymous rules are eliminated. *)

val rule: rule -> parameterized_rule
