module Make (Input : sig
  type t
  val read : string -> t
end) : sig
  val read  : string -> Input.t
  val flush : unit -> unit
end
