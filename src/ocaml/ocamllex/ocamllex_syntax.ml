(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Location

type action = Location.t

type regexp =
  | Epsilon
  | Characters
  | String
  | Eof
  | Sequence of regexp * regexp
  | Alternative of regexp * regexp
  | Star of regexp
  | Plus of regexp
  | Hash of regexp * regexp * Location.t
  | Bind of regexp * string loc
  | Named of string loc

type 'local_action clause = {
  pattern: regexp loc;
  action: 'local_action;
}

type 'local_action rule = {
  name: string loc;
  args: string loc list;
  clauses : 'local_action clause list;
}

type ('global_action, 'refill_action, 'local_action) raw_chunk =
  | Action of 'global_action
  | Rule of 'local_action rule
  | And_rule of 'local_action rule
  | Let_regexp of string loc * regexp loc
  | Refill_handler of 'refill_action
  | Syntax_error of Location.t

type source_chunk =
  (Location.t, Location.t, Location.t) raw_chunk

type 'a or_loc = ('a, Location.t) result

type parsed_chunk =
  (Parsetree.structure or_loc,
   Parsetree.expression or_loc,
   Parsetree.expression or_loc) raw_chunk
