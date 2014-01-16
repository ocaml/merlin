
module Values : module type of Merlin_parser_values

type t
type parser = t
type frame

type state = Raw_parser.state

val implementation : state
val interface : state

val from : state -> Lexing.position * Raw_parser.token * Lexing.position -> t
val feed : Lexing.position * Raw_parser.token * Lexing.position
        -> t
        -> [ `Accept of Raw_parser.semantic_value | `Step of t
           | `Reject of Raw_parser.step Raw_parser.parser ]

val dump : Format.formatter -> t -> unit

val stack : t -> frame option
val depth : frame -> int

val value : frame -> Raw_parser.semantic_value
val eq    : frame -> frame -> bool
val next  : frame -> frame option

val of_step : ?hint:MenhirUtils.witness -> Raw_parser.step Raw_parser.parser
  -> [`Accept of Raw_parser.semantic_value | `Reject of Raw_parser.step Raw_parser.parser | `Step of t]
val to_step : t -> Raw_parser.feed Raw_parser.parser option

(* Ease pattern matching on parser stack *)
type destruct = D of Raw_parser.semantic_value * destruct lazy_t
val destruct: frame -> destruct

module Integrate
    (P : sig
       type t
       val empty : t (* Base-case, empty stack *)
       val frame : frame -> t -> t (* Add frame *)
       (* Default: delta ~parent ~old:_ = frame parent *)
       val delta : frame -> parent:t -> old:(t * frame) -> t
     end) :
sig
  type t
  val empty : t
  val update : frame -> t -> t
  val value : t -> P.t

  val update' : parser -> t -> t
end

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

  val get : t -> path
  val length : t -> int

  val update' : parser -> t -> t
end
type path = Path.path

