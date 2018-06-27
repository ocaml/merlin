let to_string_list p =
  let rec aux acc = function
  | Path.Pident id -> Ident.name id :: acc
  | Path.Pdot (p, str, _) -> aux (str :: acc) p
  | _ -> assert false
  in
  aux [] p

module Ord = struct
  type t = Path.t
  let rec compare p1 p2 =
    (* must ignore position when comparing paths *)
    if p1 == p2 then 0 else
      match (p1, p2) with
        (Path.Pdot(p1, s1, _pos1), Path.Pdot(p2, s2, _pos2)) ->
        let c = compare p1 p2 in
        if c <> 0 then c else String.compare s1 s2
      | (Path.Papply(fun1, arg1), Path.Papply(fun2, arg2)) ->
        let c = compare fun1 fun2 in
        if c <> 0 then c else compare arg1 arg2
      | _ -> Pervasives.compare p1 p2

  let equal p1 p2 = compare p1 p2 = 0

  let rec hash = function
    | Path.Pident id -> Hashtbl.hash id
    | Path.Pdot (p, s, _) ->
       let h = hash p in
       Hashtbl.seeded_hash h (Hashtbl.hash s)
    | Path.Papply (p1, p2) ->
       let h1 = hash p1 and h2 = hash p2 in
       Hashtbl.seeded_hash h1 h2
end

module Map = Map.Make (Ord)
module Set = Set.Make (Ord)
module Tbl = Hashtbl.Make (Ord)
