module Lid : Set.OrderedType with type t = Longident.t Location.loc = struct
  type t = Longident.t Location.loc

  let compare_pos (p1 : Lexing.position) (p2 : Lexing.position) =
    match String.compare p1.pos_fname p2.pos_fname with
    | 0 -> Int.compare p1.pos_cnum p2.pos_cnum
    | n -> n

  let compare (t1 : t) (t2 : t) =
    match compare_pos t1.loc.loc_start t2.loc.loc_start with
    | 0 -> compare_pos t1.loc.loc_end t2.loc.loc_end
    | n -> n
end

module LidSet = Set.Make (Lid)
