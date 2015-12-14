open MenhirSdk
open CompressedBitSet

module Make (E : sig
    type t
    val of_int : int -> t
    val to_int : t -> int
  end) =
struct
  type t = CompressedBitSet.t
  type element = E.t
  let add elt t     = add (E.to_int elt) t
  let singleton elt = singleton (E.to_int elt)
  let remove elt t  = remove (E.to_int elt) t
  let fold f t x    = fold (fun elt x -> f (E.of_int elt) x) t x
  let iter f t      = iter (fun elt -> f (E.of_int elt)) t
  let elements t    = List.map E.of_int (elements t)
  let mem elt t     = mem (E.to_int elt) t
  let choose t      = E.of_int (choose t)

  let empty    = empty
  let is_empty = is_empty
  let cardinal = cardinal
  let subset   = subset
  let union    = union
  let inter    = inter
  let compare  = compare
  let equal    = equal
  let disjoint = disjoint
end
