(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

module Values : module type of Raw_parser_values

type t

(** Initialization *)

type state = Raw_parser.state

val implementation : state
val interface : state

val from : state -> Lexing.position * Raw_parser.token * Lexing.position -> t

(** Manipulation *)

(* Feed new token *)
val feed : Lexing.position * Raw_parser.token * Lexing.position
        -> ?record_comment:(string * Location.t -> unit)
        -> t
        -> [ `Accept of Raw_parser.symbol | `Step of t
           | `Reject of t ]

(* Location of top frame in stack *)
(* for recovery: approximate position of last correct construction *)
val get_guide : pop:int -> t -> int
val get_location : ?pop:int -> t -> Location.t
val get_lr0_state : t -> int
val get_lr1_state : t -> int
val get_lr0_states : t -> int List.Lazy.t
val get_lr1_states : t -> int List.Lazy.t
val last_token : t -> Raw_parser.token Location.loc

(* Just remove the state on top of the stack *)
val pop : t -> t option

(* Try to reduce the state on top of the stack *)
type termination
val termination : termination
val recover : ?endp:Lexing.position
  -> termination -> t
  -> (termination * (int * t Location.loc)) option

(** Stack inspection *)
type frame
val stack : t -> frame

module Frame : sig
  val value : frame -> Raw_parser.symbol
  val location : ?pop:int -> frame -> Location.t
  val eq    : frame -> frame -> bool
  val next  : ?n:int -> frame -> frame option

  val lr1_state : frame -> int
  val lr0_state : frame -> int
end

(* Dump internal state for debugging purpose *)
val dump : t -> Std.json
val dump_frame : frame -> Std.json

(** [find_marker] return the first frame that might be unsafe for the parser *)
val find_marker : t -> frame option

(** [has_marker ?diff t f] returns true iff f is still in t stack.
    If provided, [diff] is used to speed-up the search (amortized constant time),
    assuming that [diff] is the same parser as [t] with one more or one less
    token fed. *)
val has_marker : ?diff:(t * bool) -> t -> frame -> bool

(** Raise [Not_found] if no frame match *)
val root_frame : frame -> frame -> frame

val unroll_stack : from:frame -> root:frame -> frame list
