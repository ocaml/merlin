module Make (K : sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end) : sig
  type 'a t
  type key = K.t

  val create : int -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val find_opt : 'a t -> key -> 'a option

end