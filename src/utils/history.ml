(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

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

open Std

(** {0 History}
  * A sort of zipper: maintains and synchronizes a list of different
  * versions of an object (see ocamlmerlin.ml top comment).
  *)
type 'a t = {head: 'a List.non_empty; position: int; tail: 'a list}

(* New history *)
let initial x = {head = List.One x; position = 0; tail = []}

let of_list = function
  | x :: xs -> {head = List.One x; position = 0; tail = xs}
  | [] -> invalid_arg "History.of_list"

let to_list t =
  let rec rev acc = function
    | List.One x -> x :: acc
    | List.More (x, xs) -> rev (x :: acc) xs
  in
  rev t.tail t.head

let head x = x.head
let tail x = x.tail
let position x = x.position

(** Element to the left of the cursor
  * (if last operation was an insertion, the inserted value is returned)
  *)
let focused' (List.More (x,_) | List.One x) = x
let focused t = focused' t.head

(** Moves an arbitrary number of steps.
  *
  * May stop earlier if it reaches an end of history.
 *)
let rec move n head position tail =
  match head, tail with
  | head, (x :: tail) when n > 0 ->
    move (n - 1) (List.More (x,head)) (position + 1) tail
  | (List.More (x,head)), tail when n < 0 ->
    move (n + 1) head (position - 1) (x :: tail)
  | head, tail -> {head; position; tail}
let move n = function
  | {head; position; tail = x :: tail} when n > 0 ->
    move (n - 1) (List.More (x,head)) (position + 1) tail
  | {head = List.More (x,head); position; tail} when n < 0 ->
    move (n + 1) head (position - 1) (x :: tail)
  | t -> t

(** Seek a precise position.
  *
  * May stop earlier if it reaches an end of history.
  *)
let seek n h = move (n - position h) h

(** Move forward while item under cursor satisfy predicate *)
let rec seek_forward pred head position = function
  | x :: tail when pred (focused' head) ->
    seek_forward pred (List.More (x,head)) (position + 1) tail
  | tail -> {head; position; tail}
let seek_forward ?(last=false) pred = function
  | {head; position; tail = x :: tail} when pred (focused' head) ->
    let result = seek_forward pred (List.More (x,head)) (position + 1) tail in
    if last then
      move (-1) result
    else
      result
  | t -> t

(** Move backward while item under cursor satisfy predicate *)
let rec seek_backward pred position tail = function
  | List.More (x,head) when pred x ->
    seek_backward pred (position - 1) (x :: tail) head
  | head -> {head; position; tail}
let seek_backward ?(last=false) pred = function
  | {head = List.More (x,head); position; tail} when pred x ->
    let result = seek_backward pred (position - 1) (x :: tail) head in
    if last then
      move 1 result
    else
      result
  | t -> t

(** Adds an element to the left of the cursor:
  * insert w [..zyx|abc..] = [..zyxw|abc..] *)
let insert x h =
  {head = List.More (x, h.head); position = h.position + 1; tail = []}

(** Adds an element to the left of the cursor and keep the tail:
  * insert w [..zyx|abc..] = [..zyxw|abc] *)
let fake_insert x h =
  {head = List.More (x, h.head); position = h.position + 1; tail = h.tail}

(** Modifies focused element. *)
let modify f = function
  | {head = List.One x; position; _} ->
    {head = List.One (f x); position; tail = []}
  | {head = List.More (x,head); position; _} ->
    {head = List.More (f x, head); position; tail = []}

let append f {head = (List.One x | List.More (x,_) as head); position; _} =
  {head = List.More (f x, head); position = position + 1; tail = []}

let drop_tail t = {t with tail = []}

let reconstruct ~init ~fold h =
  let f b x' = List.More (fold x' (List.Non_empty.hd b), b) in
  let rec past tail = function
    | List.More (x,head) -> past (x :: tail) head
    | List.One x -> List.fold_left ~f ~init:(List.One (init x)) tail
  in
  let head = past [] h.head in
  let f (b,l) a =
    let b' = fold a b in
    (b', (b' :: l))
  in
  let tail = List.rev (snd (List.fold_left ~f ~init:(List.Non_empty.hd head, []) h.tail)) in
  let position = h.position in
  {head; position; tail}

let sync ~strong_check ~weak_check ~init ~strong_fold ~weak_update a = function
  | Some b ->
    let b = move (a.position - b.position) b in
    assert (b.position <= a.position);
    if b.position = a.position && strong_check (focused a) (focused b)
    then b, `Nothing_done
    else
      let rec aux worklist weaklist ha hb =
        match ha, hb with
        | List.One a, List.One b | List.More (a,_), List.More (b,_) when strong_check a b ->
          worklist, weaklist, hb
        | List.One a, List.One b when weak_check a b ->
          worklist, weaklist, List.One b
        | List.One a, List.One b ->
          worklist, [], List.One (init a)
        | List.More (a,ha), List.More (b,hb) when weak_check a b ->
          aux (a :: worklist) (b :: weaklist) ha hb
        | List.More (a,ha), List.More (_,hb) ->
          aux (a :: worklist) [] ha hb
        | _, _ -> assert false
      in
      let rec rewind worklist ha count =
        if count > 0
        then match ha with
          | List.More (a,ha) -> rewind (a :: worklist) ha (count - 1)
          | _ -> assert false
        else aux worklist [] ha b.head
      in
      let rec fast_forward worklist weaklist hb =
        match worklist, weaklist with
        | a :: worklist, b :: weaklist ->
          fast_forward worklist weaklist (List.More (weak_update a b, hb))
        | worklist, [] -> worklist, hb
        | [], _ -> assert false
      in
      let worklist, weaklist, hb = rewind [] a.head (a.position - b.position) in
      let worklist, hb = fast_forward worklist weaklist hb in
      begin match worklist with
      | [] ->
        assert (a.position = b.position);
        { b with head = hb },
        `Nothing_done
      | worklist ->
        let hb = List.fold_left worklist ~init:hb
            ~f:(fun hb a -> List.More (strong_fold a (focused' hb), hb)) in
        { head = hb; tail = []; position = a.position },
        `Updated
      end
  | None ->
    reconstruct ~init ~fold:strong_fold {a with tail = []},
    `Updated
