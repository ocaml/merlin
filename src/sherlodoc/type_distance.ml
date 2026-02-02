(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2024  Xavier Van de Woestyne <xaviervdw(_)gmail.com>
                                Arthur Wendling <arthur(_)tarides.com>


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

type step =
  | Wildcard
  | Tyname of string
  | Tyvar of int
  | Left_arrow
  | Right_arrow
  | Product of { position : int; length : int }
  | Argument of { position : int; length : int }

module P = Type_polarity

let make_path t =
  let rec aux prefix = function
    | Type_expr.Unhandled -> []
    | Type_expr.Wildcard -> [ Wildcard :: prefix ]
    | Type_expr.Tyvar x -> [ Tyvar x :: prefix ]
    | Type_expr.Arrow (a, b) ->
      List.rev_append
        (aux (Left_arrow :: prefix) a)
        (aux (Right_arrow :: prefix) b)
    | Type_expr.Tycon (constr, []) -> [ Tyname constr :: prefix ]
    | Type_expr.Tycon (constr, args) ->
      let length = String.length constr in
      let prefix = Tyname constr :: prefix in
      args
      |> List.mapi (fun position arg ->
             let prefix = Argument { position; length } :: prefix in
             aux prefix arg)
      |> List.fold_left (fun acc xs -> List.rev_append xs acc) []
    | Type_expr.Tuple args ->
      let length = List.length args in
      args
      |> List.mapi (fun position arg ->
             let prefix = Product { position; length } :: prefix in
             aux prefix arg)
      |> List.fold_left (fun acc xs -> List.rev_append xs acc) []
  in
  List.map List.rev (aux [] t)

let make_cache xs ys =
  let h = List.length xs |> succ
  and w = List.length ys |> succ
  and not_used = -1 in
  Array.make_matrix h w not_used

let skip_entry = 10
let max_distance = 10_000

let distance xs ys =
  let cache = make_cache xs ys in
  let rec memo ~xpolarity ~ypolarity i j xs ys =
    let cell = cache.(i).(j) in
    if cell >= 0 then cell
    else
      let value = aux ~xpolarity ~ypolarity i j xs ys in
      let () = cache.(i).(j) <- value in
      value
  and aux ~xpolarity ~ypolarity i j xs ys =
    match (xs, ys) with
    | [], _ -> 0
    | [ Wildcard ], _ -> 0
    | _, [] -> max_distance
    | [ Tyvar _ ], [ Wildcard ] when P.equal xpolarity ypolarity -> 0
    | [ Tyvar x ], [ Tyvar y ] when P.equal xpolarity ypolarity ->
      if Int.equal x y then 0 else 1
    | Left_arrow :: xs, Left_arrow :: ys ->
      let xpolarity = P.negate xpolarity and ypolarity = P.negate ypolarity in
      memo ~xpolarity ~ypolarity (succ i) (succ j) xs ys
    | Left_arrow :: xs, _ ->
      let xpolarity = P.negate xpolarity in
      memo ~xpolarity ~ypolarity (succ i) j xs ys
    | _, Left_arrow :: ys ->
      let ypolarity = P.negate ypolarity in
      memo ~xpolarity ~ypolarity i (succ j) xs ys
    | _, Right_arrow :: ys -> memo ~xpolarity ~ypolarity i (succ j) xs ys
    | Right_arrow :: xs, _ -> memo ~xpolarity ~ypolarity (succ i) j xs ys
    | Product { length = a; _ } :: xs, Product { length = b; _ } :: ys
    | Argument { length = a; _ } :: xs, Argument { length = b; _ } :: ys ->
      let l = abs (a - b) in
      l + memo ~xpolarity ~ypolarity (succ i) (succ j) xs ys
    | Product _ :: xs, ys -> 1 + memo ~xpolarity ~ypolarity (succ i) j xs ys
    | xs, Product _ :: ys -> 1 + memo ~xpolarity ~ypolarity i (succ j) xs ys
    | Tyname x :: xs', Tyname y :: ys' when P.equal xpolarity ypolarity -> (
      match Name_cost.distance x y with
      | None -> skip_entry + memo ~xpolarity ~ypolarity i (succ j) xs ys'
      | Some cost -> cost + memo ~xpolarity ~ypolarity (succ i) (succ j) xs' ys'
      )
    | xs, Tyname _ :: ys ->
      skip_entry + memo ~xpolarity ~ypolarity i (succ j) xs ys
    | xs, Argument _ :: ys -> memo ~xpolarity ~ypolarity i (succ j) xs ys
    | _, (Wildcard | Tyvar _) :: _ -> max_distance
  in

  let positive = P.positive in
  aux ~xpolarity:positive ~ypolarity:positive 0 0 xs ys

let make_array list =
  list |> Array.of_list
  |> Array.map (fun li ->
         let li = List.mapi (fun i x -> (x, i)) li in
         List.sort Stdlib.compare li)

let init_heuristic list =
  let used = Array.make List.(length @@ hd list) false in
  let arr = make_array list in
  let h = Array.make (succ @@ Array.length arr) 0 in
  let () = Array.sort Stdlib.compare arr in
  let () =
    for i = Array.length h - 2 downto 0 do
      let best = fst @@ List.hd arr.(i) in
      h.(i) <- h.(i + 1) + best
    done
  in
  (used, arr, h)

let replace_score best score = best := Int.min score !best

let minimize = function
  | [] -> 0
  | list ->
    let used, arr, heuristics = init_heuristic list in
    let best = ref 1000 and limit = ref 0 in
    let len_a = Array.length arr in
    let rec aux rem acc i =
      let () = incr limit in
      if !limit > max_distance then false
      else if rem <= 0 then
        let score = acc + (1000 * (len_a - i)) in
        let () = replace_score best score in
        true
      else if i >= len_a then
        let score = acc + (5 * rem) in
        let () = replace_score best score in
        true
      else if acc + heuristics.(i) >= !best then true
      else
        let rec find = function
          | [] -> true
          | (cost, j) :: rest ->
            let continue =
              if used.(j) then true
              else
                let () = used.(j) <- true in
                let continue = aux (pred rem) (acc + cost) (succ i) in
                let () = used.(j) <- false in
                continue
            in
            if continue then find rest else false
        in
        find arr.(i)
    in
    let _ = aux (Array.length used) 0 0 in
    !best

let compute ~query ~entry =
  let query = make_path query in
  let path = make_path entry in
  match (path, query) with
  | _, [] | [], _ -> 1000
  | _ -> query |> List.map (fun p -> List.map (distance p) path) |> minimize
