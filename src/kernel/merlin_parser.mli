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
        -> t
        -> [ `Step of t | `Reject ]

(* Dump internal state for debugging purpose *)
val dump : Format.formatter -> t -> unit

(* Location of top frame in stack *)
(* for recovery: approximate position of last correct construction *)
val location : t -> Location.t
val last_token : t -> Raw_parser.token Location.loc
val reached_eof : t -> bool

(* Just remove the state on top of the stack *)
val pop : t -> t option

(* Try to reduce the state on top of the stack *)
val recover : t -> t option

(* Access to underlying raw parser *)
val to_step : t -> Raw_parser.feed Raw_parser.parser option


(** Stack inspection *)
type frame

val stack : t -> frame option

module Frame : sig
  val depth : frame -> int

  val value : frame -> Raw_parser.symbol
  val location : frame -> Location.t
  val eq    : frame -> frame -> bool
  val next  : frame -> frame option

  (* Ease pattern matching on parser stack *)
  type destruct = D of Raw_parser.symbol * destruct lazy_t
  val destruct: frame -> destruct
end



(** Stack integration, incrementally compute metric over each frame *)

type parser = t

module Integrate
    (P : sig

       (* Type of the value computed at each frame *)
       type t

       (* User-defined state *)
       type st

       (* Generate an initial value, from an empty stack *)
       val empty : st -> t

       (* Fold function updating a value from a frame *)
       val frame : st -> frame -> t -> t

       (* (REMOVE?) Special case, specific fold function called
          at the point where two stacks start diverging
       *)
       val delta : st -> frame -> t -> old:(t * frame) -> t

       (* Check if an intermediate result is still valid.
          If this function returns [false], this value will not be reused
          in the incremental computation.
       *)
       val validate : st -> t -> bool

       (* [evict st t] is called when [t] is dropped out of the value stack *)
       val evict : st -> t -> unit

     end) :
sig
  type t

  (* Return a fresh incremental computation from user-defined state *)
  val empty : P.st -> t

  (* Starting from a top frame, update incremental value while minimizing
     amount of computations. *)
  val update : P.st -> frame -> t -> t

  (* Same but starting from a parser *)
  val update' : P.st -> parser -> t -> t

  (* Observe the current value computed *)
  val value : t -> P.t

  (* Drop the stack frame at the top of the computation, if any *)
  val previous : t -> t option
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

