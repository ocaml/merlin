(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

module List = struct
  include ListLabels

  let rec filter_map ~f = function
    | [] -> []
    | x :: xs ->
      match f x with
      | None -> filter_map ~f xs
      | Some x -> x :: filter_map ~f xs

  let rec map_end ~f l1 l2 =
    match l1 with
    | [] -> l2
    | hd::tl -> f hd :: map_end ~f tl l2

  let concat_map ~f l = flatten (map ~f l)

  let replicate elem n =
    let rec aux acc elem n =
      if n <= 0 then acc else aux (elem :: acc) elem (n-1)
    in
    aux [] elem n

  let rec remove x = function
    | [] -> []
    | hd :: tl when x = hd -> tl
    | hd :: tl -> hd :: remove x tl

  let rec remove_all x = function
    | [] -> []
    | hd :: tl when x = hd -> remove_all x tl
    | hd :: tl -> hd :: remove_all x tl

  let rec same ~f l1 l2 = match l1, l2 with
    | [], [] -> true
    | (hd1 :: tl1), (hd2 :: tl2) when f hd1 hd2 -> same ~f tl1 tl2
    | _, _ -> false

  (* [length_lessthan n l] returns
   *   Some (List.length l) if List.length l <= n
   *   None otherwise *)
  let length_lessthan n l =
    let rec aux i = function
      | _ :: xs when i < n -> aux (succ i) xs
      | [] -> Some i
      | _ -> None
    in
    aux 0 l

  let filter_dup lst =
    let tbl = Hashtbl.create 17 in
    let f a b =
      if Hashtbl.mem tbl b
      then a
      else (Hashtbl.add tbl b (); b :: a)
    in
    rev (fold_left ~f ~init:[] lst)

  let rec drop_while ~f = function
    | x :: xs when f x -> drop_while ~f xs
    | xs -> xs

  module Lazy = struct
    type 'a t =
      | Nil
      | Cons of 'a * 'a t lazy_t

    let rec map ~f = function
      | Nil -> Nil
      | Cons (hd,tl) ->
         Cons (hd, lazy (map ~f (Lazy.force tl)))

    let rec to_strict = function
      | Nil -> []
      | Cons (hd, lazy tl) -> hd :: to_strict tl
  end
end

module Option = struct
  let bind opt ~f =
    match opt with
    | None -> None
    | Some x -> f x

  let map ~f = function
    | None -> None
    | Some x -> Some (f x)

  let value ~default = function
    | None -> default
    | Some x -> x

  let value_map ~f ~default = function
    | None -> default
    | Some x -> f x

  let iter ~f = function
    | None -> ()
    | Some x -> f x

  module Infix = struct
    let return x  = Some x
    let (>>=) x f = bind x ~f
  end

  include Infix
end

module String = struct
  include StringLabels

  (* [is_prefixed ~by s] returns [true] iff [by] is a prefix of [s] *)
  let is_prefixed ~by =
    let l = String.length by in
    fun s ->
    let l' = String.length s in
    (l' >= l) &&
      (try for i = 0 to pred l do
             if s.[i] <> by.[i] then
               raise Not_found
           done;
           true
       with Not_found -> false)

  (* Drop characters from beginning of string *)
  let drop n s = sub s n (length s - n)
end

let sprintf = Printf.sprintf

module Format = struct
  include Format

  let to_string ?(width=0) () =
    let b = Buffer.create 32 in
    let ppf = formatter_of_buffer b in
    let contents () =
      pp_print_flush ppf ();
      Buffer.contents b
    in
    pp_set_margin ppf width;
    ppf, contents
end

module Lexing = struct
  include Lexing

  let move buf p =
    buf.lex_abs_pos <- (p.pos_cnum - buf.lex_curr_pos);
    buf.lex_curr_p <- p

  let from_strings ?position source refill =
    let pos = ref 0 in
    let len = ref (String.length source) in
    let source = ref source in
    let lex_fun buf size =
      let count = min (!len - !pos) size in
      let count =
        if count <= 0 then
          begin
            source := refill ();
            len := String.length !source;
            pos := 0;
            min !len size
          end
        else count
      in
      if count <= 0 then 0
      else begin
          String.blit !source !pos buf 0 count;
          pos := !pos + count;
          count
        end
    in
    let buf = from_function lex_fun in
    Option.iter ~f:(move buf) position;
    buf

  (* Manipulating position *)
  let make_pos (pos_lnum, pos_cnum) =
    Lexing.({ pos_fname = "" ; pos_lnum ; pos_cnum ; pos_bol = 0 })

  let split_pos pos = Lexing.(pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

  let compare_pos p1 p2 =
    compare (split_pos p1) (split_pos p2)
end

module Stream = struct
  include Stream
  let map ~f s =
    from (fun _ ->
      try Some (f (next s))
      with Failure -> None)
end

module Either = struct
  type ('a,'b) t = L of 'a | R of 'b

  let elim f g = function
    | L a -> f a
    | R b -> g b

  let try' f =
    try R (f ())
    with exn -> L exn

  (* Remove ? *)
  let join (exns, r) = match r with
    | L e -> (exns, L e)
    | R (exns', r') -> (exns @ exns'), r'

  let split =
    let rec aux l1 l2 = function
      | L a :: l -> aux (a :: l1) l2 l
      | R b :: l -> aux l1 (b :: l2) l
      | [] -> List.rev l1, List.rev l2
    in
    fun l -> aux [] [] l
end
type ('a, 'b) either = ('a, 'b) Either.t
type 'a or_exn = (exn, 'a) Either.t

(* Simple list zipper *)
module Zipper = struct
  type 'a t = Zipper of 'a list * int * 'a list

  let rec shift n = function
    | Zipper (prev, pos, a :: next) when n > 0 ->
      shift (pred n) (Zipper (a :: prev, succ pos, next))
    | Zipper (a :: prev, pos, next) when n < 0 ->
      shift (succ n) (Zipper (prev, pred pos, a :: next))
    | zipper -> zipper

  let of_list l = Zipper ([], 0, l)
  let insert a (Zipper (prev, pos, next)) =
    Zipper (a :: prev, succ pos, next)

  let seek n (Zipper (_,pos,_) as z) =
    shift (n - pos) z

  let change_tail next (Zipper (prev,pos,_next)) =
    Zipper (prev,pos,next)
end

type 'a zipper = 'a Zipper.t = Zipper of 'a list * int * 'a list

(* Dynamic binding pattern *)
module Fluid : sig
  type 'a t
  val from : 'a -> 'a t
  val from_ref : 'a ref -> 'a t
  val let' : 'a t -> 'a -> (unit -> 'b) -> 'b
  val get : 'a t -> 'a
end = struct
  type 'a t = 'a ref
  let from x = ref x
  let from_ref x = x
  let let' d v f =
    let p = !d in
    d := v;
    try let r = f () in
        d := p; r
    with exn ->
      d := p; raise exn

  let get x = !x
end

type 'a fluid = 'a Fluid.t
let fluid = Fluid.from
let (~!) = Fluid.get

module Sync : sig
  type 'a t
  val none : unit -> 'a t
  val make : 'a -> 'a t
  val same : 'a -> 'a t -> bool
end = struct
  type 'a t = 'a Weak.t
  let make x =
    let t = Weak.create 1 in
    Weak.set t 0 (Some x);
    t
  let same x t =
    match Weak.get t 0 with
    | None -> false
    | Some x' -> x == x'

  let none : exn t = make Not_found
  let none () : 'a t = Obj.magic none
end

module Json = Yojson.Basic
