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
  type key
  type 'a t

  val empty : unit -> 'a t
  val bindings : 'a t -> (key * 'a) list
  val add : key -> 'a -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val cardinal : 'a t -> int
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val choose_opt : 'a t -> (key * 'a) option
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val schema :
    'a t Type.Id.t ->
    Granular_marshal.iter ->
    (Granular_marshal.iter -> key -> 'a -> unit) ->
    'a t ->
    unit
end

module Make (Ord : Map.OrderedType) = struct
  type key = Ord.t
  type 'a t = 'a s link
  and 'a s = Empty | Node of { l : 'a t; v : key; d : 'a; r : 'a t; h : int }

  let empty () = link Empty

  let height s =
    match fetch s with
    | Empty -> 0
    | Node { h; _ } -> h

  let create (l : 'a t) x d (r : 'a t) : 'a t =
    let hl = height l and hr = height r in
    link (Node { l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1) })

  let singleton x d =
    let empty = empty () in
    link (Node { l = empty; v = x; d; r = empty; h = 1 })

  let bal (l : 'a t) x d (r : 'a t) : 'a t =
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
      | Empty -> invalid_arg "Map.bal"
      | Node { l = ll; v = lv; d = ld; r = lr; _ } ->
        if height ll >= height lr then create ll lv ld (create lr x d r)
        else begin
          match fetch lr with
          | Empty -> invalid_arg "Map.bal"
          | Node { l = lrl; v = lrv; d = lrd; r = lrr; _ } ->
            create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
    end
    else if hr > hl + 2 then begin
      match fetch r with
      | Empty -> invalid_arg "Map.bal"
      | Node { l = rl; v = rv; d = rd; r = rr; _ } ->
        if height rr >= height rl then create (create l x d rl) rv rd rr
        else begin
          match fetch rl with
          | Empty -> invalid_arg "Map.bal"
          | Node { l = rll; v = rlv; d = rld; r = rlr; _ } ->
            create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
    end
    else
      link (Node { l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1) })

  let rec bindings_aux accu s =
    match fetch s with
    | Empty -> accu
    | Node { l; v; d; r; _ } -> bindings_aux ((v, d) :: bindings_aux accu r) l

  let bindings t = bindings_aux [] t

  let is_empty s =
    match fetch s with
    | Empty -> true
    | _ -> false

  let rec add x data s : 'a t =
    match fetch s with
    | Empty -> link (Node { l = s; v = x; d = data; r = s; h = 1 })
    | Node { l; v; d; r; h } ->
      let c = Ord.compare x v in
      if c = 0 then
        if d == data then s else link (Node { l; v = x; d = data; r; h })
      else if c < 0 then
        let ll = add x data l in
        if l == ll then s else bal ll v d r
      else
        let rr = add x data r in
        if r == rr then s else bal l v d rr

  let rec find x s =
    match fetch s with
    | Empty -> raise Not_found
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then d else find x (if c < 0 then l else r)

  let rec find_opt x s =
    match fetch s with
    | Empty -> None
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then Some d else find_opt x (if c < 0 then l else r)

  let rec mem x s =
    match fetch s with
    | Empty -> false
    | Node { l; v; r; _ } ->
      let c = Ord.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec min_binding (t : 'a t) : key * 'a =
    match fetch t with
    | Empty -> raise Not_found
    | Node { l; v; d; _ } when fetch l = Empty -> (v, d)
    | Node { l; _ } -> min_binding l

  let choose_opt t = try Some (min_binding t) with Not_found -> None

  let rec remove_min_binding (t : 'a t) : 'a t =
    match fetch t with
    | Empty -> invalid_arg "Map.remove_min_elt"
    | Node { l; r; _ } when fetch l = Empty -> r
    | Node { l; v; d; r; _ } -> bal (remove_min_binding l) v d r

  let merge (t1 : 'a t) (t2 : 'a t) : 'a t =
    match (fetch t1, fetch t2) with
    | Empty, _t -> t2
    | _t, Empty -> t1
    | _, _ ->
      let x, d = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

  let rec remove x s : 'a t =
    match fetch s with
    | Empty -> s
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then merge l r
      else if c < 0 then
        let ll = remove x l in
        if l == ll then s else bal ll v d r
      else
        let rr = remove x r in
        if r == rr then s else bal l v d rr

  let rec iter f s =
    match fetch s with
    | Empty -> ()
    | Node { l; v; d; r; _ } ->
      iter f l;
      f v d;
      iter f r

  let rec map f s =
    match fetch s with
    | Empty -> empty ()
    | Node { l; v; d; r; h } ->
      let l' = map f l in
      let d' = f d in
      let r' = map f r in
      link (Node { l = l'; v; d = d'; r = r'; h })

  let rec fold f m accu =
    match fetch m with
    | Empty -> accu
    | Node { l; v; d; r; _ } -> fold f r (f v d (fold f l accu))

  let rec add_min_binding k x s =
    match fetch s with
    | Empty -> singleton k x
    | Node { l; v; d; r; _ } -> bal (add_min_binding k x l) v d r

  let rec add_max_binding k x s =
    match fetch s with
    | Empty -> singleton k x
    | Node { l; v; d; r; _ } -> bal l v d (add_max_binding k x r)

  let rec join (l : 'a t) v d (r : 'a t) =
    match (fetch l, fetch r) with
    | Empty, _ -> add_min_binding v d r
    | _, Empty -> add_max_binding v d l
    | ( Node { l = ll; v = lv; d = ld; r = lr; h = lh },
        Node { l = rl; v = rv; d = rd; r = rr; h = rh } ) ->
      if lh > rh + 2 then bal ll lv ld (join lr v d r)
      else if rh > lh + 2 then bal (join l v d rl) rv rd rr
      else create l v d r

  let concat (t1 : 'a t) (t2 : 'a t) : 'a t =
    match (fetch t1, fetch t2) with
    | Empty, _t -> t2
    | _t, Empty -> t1
    | _, _ ->
      let x, d = min_binding t2 in
      join t1 x d (remove_min_binding t2)

  let concat_or_join t1 v d t2 =
    match d with
    | Some d -> join t1 v d t2
    | None -> concat t1 t2

  let rec split x s =
    match fetch s with
    | Empty -> (s, None, s)
    | Node { l; v; d; r; _ } ->
      let c = Ord.compare x v in
      if c = 0 then (l, Some d, r)
      else if c < 0 then
        let ll, pres, rl = split x l in
        (ll, pres, join rl v d r)
      else
        let lr, pres, rr = split x r in
        (join l v d lr, pres, rr)

  let rec union f (s1 : 'a t) (s2 : 'a t) : 'a t =
    match (fetch s1, fetch s2) with
    | _, Empty -> s1
    | Empty, _ -> s2
    | ( Node { l = l1; v = v1; d = d1; r = r1; h = h1 },
        Node { l = l2; v = v2; d = d2; r = r2; h = h2 } ) -> (
      if h1 >= h2 then
        let l2, d2, r2 = split v1 s2 in
        let l = union f l1 l2 and r = union f r1 r2 in
        match d2 with
        | None -> join l v1 d1 r
        | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
      else
        let l1, d1, r1 = split v2 s1 in
        let l = union f l1 l2 and r = union f r1 r2 in
        match d1 with
        | None -> join l v2 d2 r
        | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r)

  let rec cardinal s =
    match fetch s with
    | Empty -> 0
    | Node { l; r; _ } -> cardinal l + 1 + cardinal r

  let rec update x f t =
    match fetch t with
    | Empty -> begin
      match f None with
      | None -> t
      | Some data -> link (Node { l = t; v = x; d = data; r = t; h = 1 })
    end
    | Node { l; v; d; r; h } ->
      let c = Ord.compare x v in
      if c = 0 then begin
        match f (Some d) with
        | None -> merge l r
        | Some data ->
          if d == data then t else link (Node { l; v = x; d = data; r; h })
      end
      else if c < 0 then
        let ll = update x f l in
        if l == ll then t else bal ll v d r
      else
        let rr = update x f r in
        if r == rr then t else bal l v d rr

  let rec schema type_id iter f m =
    iter.yield m type_id @@ fun iter tree ->
    match tree with
    | Empty -> ()
    | Node { l; v; d; r; _ } ->
      schema type_id iter f l;
      f iter v d;
      schema type_id iter f r
end
