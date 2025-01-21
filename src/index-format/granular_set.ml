(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Granular_marshal

module type S = sig
  type elt
  type t

  val empty : t
  val add : elt -> t -> t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val singleton : elt -> t
  val remove : elt -> t -> t
  val filter : (elt -> bool) -> t -> t
  val union : t -> t -> t
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val cardinal : t -> int
  val elements : t -> elt list
  val schema :
    Granular_marshal.iter -> (Granular_marshal.iter -> elt -> unit) -> t -> unit
end

module Make (Ord : Set.OrderedType) = struct
  type elt = Ord.t

  type t = s link
  and s = Empty | Node of { l : t; v : elt; r : t; h : int }

  let height t =
    match fetch t with
    | Empty -> 0
    | Node { h; _ } -> h

  let create (l : t) v (r : t) : t =
    let hl =
      match fetch l with
      | Empty -> 0
      | Node { h; _ } -> h
    in
    let hr =
      match fetch r with
      | Empty -> 0
      | Node { h; _ } -> h
    in
    link (Node { l; v; r; h = (if hl >= hr then hl + 1 else hr + 1) })

  let bal (l : t) v (r : t) =
    let hl =
      match fetch l with
      | Empty -> 0
      | Node { h; _ } -> h
    in
    let hr =
      match fetch r with
      | Empty -> 0
      | Node { h; _ } -> h
    in
    if hl > hr + 2 then begin
      match fetch l with
      | Empty -> invalid_arg "Set.bal"
      | Node { l = ll; v = lv; r = lr; _ } ->
        if height ll >= height lr then create ll lv (create lr v r)
        else begin
          match fetch lr with
          | Empty -> invalid_arg "Set.bal"
          | Node { l = lrl; v = lrv; r = lrr; _ } ->
            create (create ll lv lrl) lrv (create lrr v r)
        end
    end
    else if hr > hl + 2 then begin
      match fetch r with
      | Empty -> invalid_arg "Set.bal"
      | Node { l = rl; v = rv; r = rr; _ } ->
        if height rr >= height rl then create (create l v rl) rv rr
        else begin
          match fetch rl with
          | Empty -> invalid_arg "Set.bal"
          | Node { l = rll; v = rlv; r = rlr; _ } ->
            create (create l v rll) rlv (create rlr rv rr)
        end
    end
    else link (Node { l; v; r; h = (if hl >= hr then hl + 1 else hr + 1) })

  let empty = link Empty

  let rec add x t : t =
    match fetch t with
    | Empty -> link (Node { l = link Empty; v = x; r = link Empty; h = 1 })
    | Node { l; v; r; _ } as t ->
      let c = Ord.compare x v in
      if c = 0 then link t
      else if c < 0 then
        let ll = add x l in
        if l == ll then link t else bal ll v r
      else
        let rr = add x r in
        if r == rr then link t else bal l v rr

  let singleton x = link (Node { l = link Empty; v = x; r = link Empty; h = 1 })

  let rec min_elt t =
    match fetch t with
    | Empty -> raise Not_found
    | Node { l; v; _ } when fetch l = Empty -> v
    | Node { l; _ } -> min_elt l

  let rec remove_min_elt t =
    match fetch t with
    | Empty -> invalid_arg "Set.remove_min_elt"
    | Node { l; r; _ } when fetch l = Empty -> r
    | Node { l; v; r; _ } -> bal (remove_min_elt l) v r

  let merge t1 t2 =
    match (fetch t1, fetch t2) with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _, _ -> bal t1 (min_elt t2) (remove_min_elt t2)

  let is_empty t =
    match fetch t with
    | Empty -> true
    | _ -> false

  let rec mem x t =
    match fetch t with
    | Empty -> false
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec remove x t =
    match fetch t with
    | Empty -> link Empty
    | Node { l; v; r; _ } as t ->
      let c = Ord.compare x v in
      if c = 0 then merge l r
      else if c < 0 then
        let ll = remove x l in
        if l == ll then link t else bal ll v r
      else
        let rr = remove x r in
        if r == rr then link t else bal l v rr

  let rec add_min_element x t =
    match fetch t with
    | Empty -> singleton x
    | Node { l; v; r; _ } -> bal (add_min_element x l) v r

  let rec add_max_element x t =
    match fetch t with
    | Empty -> singleton x
    | Node { l; v; r; _ } -> bal l v (add_max_element x r)

  let rec join (l : t) v (r : t) =
    match (fetch l, fetch r) with
    | Empty, _ -> add_min_element v r
    | _, Empty -> add_max_element v l
    | ( Node { l = ll; v = lv; r = lr; h = lh },
        Node { l = rl; v = rv; r = rr; h = rh } ) ->
      if lh > rh + 2 then bal ll lv (join lr v r)
      else if rh > lh + 2 then bal (join l v rl) rv rr
      else create l v r

  let rec max_elt t =
    match fetch t with
    | Empty -> raise Not_found
    | Node { v; r; _ } when fetch r = Empty -> v
    | Node { r; _ } -> max_elt r

  let concat t1 t2 =
    match (fetch t1, fetch t2) with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _, _ -> join t1 (min_elt t2) (remove_min_elt t2)

  let rec split x t =
    match fetch t with
    | Empty -> (link Empty, false, link Empty)
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let ll, pres, rl = split x l in
        (ll, pres, join rl v r)
      else
        let lr, pres, rr = split x r in
        (join l v lr, pres, rr)

  let rec union t1 t2 =
    match (fetch t1, fetch t2) with
    | Empty, _ -> t2
    | _, Empty -> t1
    | ( Node { l = l1; v = v1; r = r1; h = h1 },
        Node { l = l2; v = v2; r = r2; h = h2 } ) ->
      if h1 >= h2 then
        if h2 = 1 then add v2 t1
        else begin
          let l2, _, r2 = split v1 t2 in
          join (union l1 l2) v1 (union r1 r2)
        end
      else if h1 = 1 then add v1 t2
      else begin
        let l1, _, r1 = split v2 t1 in
        join (union l1 l2) v2 (union r1 r2)
      end

  let rec filter p t =
    match fetch t with
    | Empty -> link Empty
    | Node { l; v; r; _ } as t ->
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then if l == l' && r == r' then link t else join l' v r'
      else concat l' r'

  let rec cardinal t =
    match fetch t with
    | Empty -> 0
    | Node { l; r; _ } -> cardinal l + 1 + cardinal r

  let rec elements_aux accu t =
    match fetch t with
    | Empty -> accu
    | Node { l; v; r; _ } -> elements_aux (v :: elements_aux accu r) l

  let elements s = elements_aux [] s

  let try_join l v r =
    if
      (fetch l = Empty || Ord.compare (max_elt l) v < 0)
      && (fetch r = Empty || Ord.compare v (min_elt r) < 0)
    then join l v r
    else union l (add v r)

  let rec map f t =
    match fetch t with
    | Empty -> link Empty
    | Node { l; v; r; _ } as t ->
      let l' = map f l in
      let v' = f v in
      let r' = map f r in
      if l == l' && v == v' && r == r' then link t else try_join l' v' r'

  let rec iter f t =
    match fetch t with
    | Empty -> ()
    | Node { l; v; r; _ } ->
      iter f l;
      f v;
      iter f r

  let type_id = Type.Id.make ()

  let rec schema iter f m =
    iter.yield m type_id @@ fun iter tree ->
    match tree with
    | Empty -> ()
    | Node { l; v; r; _ } ->
      schema iter f l;
      f iter v;
      schema iter f r
end
