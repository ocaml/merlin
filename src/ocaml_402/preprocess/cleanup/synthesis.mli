open MenhirSdk.Cmly_format

type variable =
  | Head of lr1_state * nonterminal
  | Tail of lr1_state * production * int

type 'a paction =
  | Abort
  | Reduce of production
  | Shift  of symbol
  | Var    of 'a

type action = variable paction

module type Grammar = sig
  val grammar         : grammar
  val cost_of_prod    : production -> float
  val penalty_of_item : production * int -> float
  val cost_of_symbol  : symbol -> float
end

module type Solution = sig
  val grammar         : grammar
  val cost_of : variable -> float
  val cost_of_action : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report : Format.formatter -> unit
end

module Make (G : Grammar) : Solution
