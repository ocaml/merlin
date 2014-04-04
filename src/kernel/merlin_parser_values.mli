type terminal = Raw_parser.Query.terminal

module Token : sig
  type t = Raw_parser.token
  (*type index = private int

  val to_index: t -> index
  val of_index: index -> t

  val to_terminal: t -> terminal
  val of_terminal: terminal -> t option*)
  val to_string: t -> string
  (*val all: t array*)
end

module Nonterminal : sig
  type t = Raw_parser.nonterminal
(*  type index = private int

  val to_index: t -> index
  val of_index: index -> t

  val to_terminal: t -> terminal
  val of_terminal: terminal -> t option*)
  val to_string: t -> string
  (*val all: t array*)
end

module Value : sig
  type t = Raw_parser.semantic_value =
    | Bottom
    | Terminal of Token.t
    | Nonterminal of Nonterminal.t

  (*val to_terminal: t -> terminal
  val of_terminal: terminal -> t*)
  val to_string: t -> string
  (*val all: t array*)
end
