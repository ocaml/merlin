(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

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
