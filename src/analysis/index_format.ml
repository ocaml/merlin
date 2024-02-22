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

(** [add tbl uid locs] adds a binding of [uid] to the locations [locs]. If this key is
    already present the locations are merged. *)
    let add tbl uid locs =
      try
        let locations = Hashtbl.find tbl uid in
        Hashtbl.replace tbl uid (LidSet.union locs locations)
      with Not_found -> Hashtbl.add tbl uid locs
