open MenhirSdk.Cmly_format

type item = lr1_state * production * int

type recovery = lr1_state -> int * (lr1_state option * item list) list

module Make (G : Synthesis.Solution) : sig
  val recover : recovery
  val report : Format.formatter -> unit
end
