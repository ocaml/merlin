module Codeconsing (G : Synthesis.Solution) : sig

  (* Step 1: record all definitions *)
  val record_items : Recovery.item list -> unit

  (* Step 2: get prelude maximizing & serialization function *)
  val normalize : unit -> (string * string) list * (Recovery.item list -> string)

end

module Make (G : Synthesis.Solution) : sig
  val emit : Recovery.recovery -> Format.formatter -> unit
end
