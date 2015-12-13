(* A compressed (or should we say sparse?) bit set is a list of pairs
   of integers. The first component of every pair is an index, while
   the second component is a bit field. The list is sorted by order
   of increasing indices. *)

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

type set = t
type element = int

let word_size =
  Sys.word_size - 1

let empty =
  N

let is_empty = function
  | N ->
    true
  | C _ ->
    false

let add i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec add = function
    | N ->
      (* Insert at end. *)
      C (iaddr, imask, N)
    | C (addr, ss, qs) as s ->
      if iaddr < addr then
        (* Insert in front. *)
        C (iaddr, imask, s)
      else if iaddr = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = ss lor imask in
        if ss' = ss then
          s
        else
          C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = add qs in
        if qs == qs' then
          s
        else
          C (addr, ss, qs')
  in
  add s

let singleton i =
  add i N

let remove i s =
  let ioffset = i mod word_size in
  let iaddr = i - ioffset
  and imask = 1 lsl ioffset in
  let rec remove = function
    | N ->
      N
    | C (addr, ss, qs) as s ->
      if iaddr < addr then
        s
      else if iaddr = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = ss land (lnot imask) in
        if ss' = 0 then
          qs
        else if ss' = ss then
          s
        else
          C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = remove qs in
        if qs == qs' then
          s
        else
          C (addr, ss, qs')
  in
  remove s

let rec fold f s accu =
  match s with
  | N ->
    accu
  | C (base, ss, qs) ->
    let limit = base + word_size in
    let rec loop i ss accu =
      if i = limit then
        accu
      else
        loop (i + 1) (ss lsr 1) (if ss land 1 = 1 then f i accu else accu)
    in
    fold f qs (loop base ss accu)

let iter f s =
  fold (fun x () -> f x) s ()

let cardinal s =
  fold (fun _ m -> m + 1) s 0

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
    true
  | _, N ->
    false
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then
      false
    else if addr1 = addr2 then
      if (ss1 land ss2) <> ss1 then
        false
      else
        subset qs1 qs2
    else
      subset s1 qs2

let mem i s =
  subset (singleton i) s

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
    s
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then
      C (addr1, ss1, union qs1 s2)
    else if addr1 > addr2 then
      let s = union s1 qs2 in
      if s == qs2 then
        s2
      else
        C (addr2, ss2, s)
    else
      let ss = ss1 lor ss2 in
      let s = union qs1 qs2 in
      if ss == ss2 && s == qs2 then
        s2
      else
        C (addr1, ss, s)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
    N
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then
      inter qs1 s2
    else if addr1 > addr2 then
      inter s1 qs2
    else
      let ss = ss1 land ss2 in
      let s = inter qs1 qs2 in
      if ss = 0 then
        s
      else
      if (ss = ss1) && (s == qs1) then
        s1
      else
        C (addr1, ss, s)

exception Found of int

let choose s =
  try
    iter (fun x ->
        raise (Found x)
      ) s;
    raise Not_found
  with Found x ->
    x

let rec compare s1 s2 =
  match s1, s2 with
    N, N ->  0
  | _, N ->  1
  | N, _ -> -1
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 < addr2 then -1
    else if addr1 > addr2 then 1
    else if ss1 < ss2 then -1
    else if ss1 > ss2 then 1
    else compare qs1 qs2

let equal s1 s2 =
  compare s1 s2 = 0

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
    true
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
    if addr1 = addr2 then
      if (ss1 land ss2) = 0 then
        disjoint qs1 qs2
      else
        false
    else if addr1 < addr2 then
      disjoint qs1 s2
    else
      disjoint s1 qs2

module Make (E : sig
    type t
    val of_int : int -> t
    val to_int : t -> int
  end) =
struct
  type t = set
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
