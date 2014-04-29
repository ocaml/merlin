(** Helpers around Menhir generated definitions *)

module Token : sig
  type t = Raw_parser.token
  val to_string: t -> string
end

module Nonterminal : sig
  type t = Raw_parser.nonterminal
  val to_string: t -> string
end

type t = Raw_parser.semantic_value =
  | Bottom
  | Terminal of Token.t
  | Nonterminal of Nonterminal.t
val to_string: t -> string
