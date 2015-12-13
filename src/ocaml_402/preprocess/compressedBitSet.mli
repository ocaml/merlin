module type S = sig
  type t
  type element
  val empty : t
  val is_empty : t -> bool
  val add : element -> t -> t
  val singleton : element -> t
  val remove : element -> t -> t
  val fold : (element -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (element -> unit) -> t -> unit
  val cardinal : t -> int
  val elements : t -> element list
  val subset : t -> t -> bool
  val mem : element -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val choose : t -> element
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val disjoint : t -> t -> bool
end

type t =
  | N
  | C of int * int * t

type element = int

include S with type t := t and type element := element

module Make (E : sig
    type t
    val of_int : int -> t
    val to_int : t -> int
  end)
  : S with type element := E.t
