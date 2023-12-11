type s = private ..

type t = private A 

type t = private A of int

type p = private { x:int }

type t = private int

module N : sig
  type t = private int
  val of_int: int -> t
  val to_int: t -> int
end = struct
  type t = int
  let of_int n = assert (n >= 0); n
  let to_int n = n
end