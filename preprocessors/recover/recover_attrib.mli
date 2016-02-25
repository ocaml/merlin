module type S = sig
  module G : Utils.G

  val cost_of_prod    : G.production -> float
  val penalty_of_item : G.production * int -> float
  val cost_of_symbol  : G.symbol -> float

  val default_prelude     : Format.formatter -> unit
  val default_terminal    : G.terminal -> string option
  val default_nonterminal : G.nonterminal -> string option
end

module Make (G : Utils.G) : S with module G = G
