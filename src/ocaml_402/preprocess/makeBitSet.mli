module Make (E : sig
    type t
    val of_int : int -> t
    val to_int : t -> int
  end)
  : MenhirSdk.GSet.S with type element := E.t
