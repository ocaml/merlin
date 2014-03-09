module Values : module type of Merlin_parser_values

type t
type parser = t
type frame

(** Initialization *)
type state = Raw_parser.state
val implementation : state
val interface : state
val from : state -> Lexing.position * Raw_parser.token * Lexing.position -> t

(** Manipulation *)

(* Feed new token *)
val feed : Lexing.position * Raw_parser.token * Lexing.position
        -> t
        -> [ `Accept of Raw_parser.semantic_value | `Step of t
           | `Reject of Raw_parser.step Raw_parser.parser ]

(* Dump internal state for debugging purpose *)
val dump : Format.formatter -> t -> unit

(* Location of top frame in stack *)
(* for recovery: approximate position of last correct construction *)
val location : t -> Location.t
val last_token : t -> Raw_parser.token Location.loc

(* Stack unwinding, hopefully to find a recovery point *)
val pop : t -> t option

(* Try to feed a RECOVER token *)
(* succeeds if it's safe to recover from current state *)
val recover : ?location:Location.t -> t -> t Location.loc option
val reconstruct : exn -> t -> t option

(* Access to underlying raw parser *)
val to_step : t -> Raw_parser.feed Raw_parser.parser

(** Stack inspection *)
module Frame : sig
  val stack : t -> frame option
  val depth : frame -> int

  val value : frame -> Raw_parser.semantic_value
  val location : frame -> Location.t
  val eq    : frame -> frame -> bool
  val next  : frame -> frame option

  (* Ease pattern matching on parser stack *)
  type destruct = D of Raw_parser.semantic_value * destruct lazy_t
  val destruct: frame -> destruct
end

(** Stack integration, incrementally compute metric over each frame *)
module Integrate
    (P : sig
       (* Arbitrary state, passed to update functions *)
       type st
       type t
       val empty : st -> t (* Base-case, empty stack *)
       val frame : st -> frame -> t -> t (* Add frame *)
       (* Default: delta st f t ~old:_ = frame st f t *)
       val delta : st -> frame -> t -> old:(t * frame) -> t
       (* Check if an intermediate result is still valid *)
       val validate : st -> t -> bool
       (* [evict st t] is called when [t] is no longer sync *)
       val evict : st -> t -> unit
     end) :
sig
  type t

  val empty : P.st -> t
  val update : P.st -> frame -> t -> t
  val update' : P.st -> parser -> t -> t

  val value : t -> P.t
end

(** A basic metric: path leading to an item *)
module Path : sig
  type item =
    | Let of Asttypes.rec_flag * int
    | Struct of int
    | Sig of int
    | Module_rec of int
    | Object of int
    | Class of int

  type path = item list

  type t
  val empty : t
  val update : frame -> t -> t
  val update' : parser -> t -> t

  val get : t -> path
  val length : t -> int
end
type path = Path.path

