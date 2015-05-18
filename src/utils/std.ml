(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

module Json = struct
  include Yojson.Basic
  let string x = `String x
  let int x = `Int x
end
type json = Json.json

module Hashtbl = struct
  include Hashtbl

  let find_some tbl key =
    try Some (find tbl key)
    with Not_found -> None

  let elements tbl = Hashtbl.fold (fun _key elt acc -> elt :: acc) tbl []
end

module List = struct
  include ListLabels

  let init ~f n =
    let rec aux i = if i = n then [] else f i :: aux (succ i) in
    aux 0

  let index ~f l =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: _ when f x -> i
      | _ :: xs -> aux (succ i) xs
    in
    aux 0 l

  let find_some ~f l =
    try Some (find ~f l)
    with Not_found -> None

  let rec rev_scan_left acc ~f l ~init = match l with
    | [] -> acc
    | x :: xs ->
      let init = f x init in
      rev_scan_left (init :: acc) ~f xs ~init

  let scan_left ~f l ~init =
      List.rev (rev_scan_left [] ~f l ~init)

  let rev_filter ~f lst =
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> aux (if f x then x :: acc else acc) xs
    in
    aux [] lst

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

  let rec remove ?(phys=false) x =
    let check = if phys then (==) else (=) in
    function
    | [] -> []
    | hd :: tl when check x hd -> tl
    | hd :: tl -> hd :: remove ~phys x tl

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

  let filter_dup' ~equiv lst =
    let tbl = Hashtbl.create 17 in
    let f a b =
      let b' = equiv b in
      if Hashtbl.mem tbl b'
      then a
      else (Hashtbl.add tbl b' (); b :: a)
    in
    rev (fold_left ~f ~init:[] lst)

  let filter_dup lst = filter_dup' ~equiv:(fun x -> x) lst

  let rec merge_cons ~f = function
    | a :: ((b :: tl) as tl') ->
      begin match f a b with
        | Some a' -> merge_cons ~f (a' :: tl)
        | None -> a :: merge_cons ~f tl'
      end
    | tl -> tl

  let rec take_while ~f = function
    | x :: xs when f x -> x :: take_while ~f xs
    | _ -> []

  let rec drop_while ~f = function
    | x :: xs when f x -> drop_while ~f xs
    | xs -> xs

  let rec take_n acc n = function
    | x :: xs when n > 0 -> take_n (x :: acc) (n - 1) xs
    | _ -> List.rev acc
  let take_n n l = take_n [] n l

  let rec drop_n n = function
    | x :: xs when n > 0 -> drop_n (n - 1) xs
    | xs -> xs

  let rec split_n acc n = function
    | x :: xs when n > 0 -> split_n (x :: acc) (n - 1) xs
    | xs -> List.rev acc, xs
  let split_n n l = split_n [] n l

  let rec split3 xs ys zs = function
    | (x,y,z) :: tl -> split3 (x :: xs) (y :: ys) (z :: zs) tl
    | [] -> List.rev xs, List.rev ys, List.rev zs
  let split3 l = split3 [] [] [] l

  let rec unfold ~f a = match f a with
    | None -> []
    | Some a -> a :: unfold f a

  let rec rev_unfold acc ~f a = match f a with
    | None -> acc
    | Some a -> rev_unfold (a :: acc) ~f a

  let rec fold_n_map ~f ~init = function
    | [] -> init, []
    | x :: xs ->
      let acc, x' = f init x in
      let acc, xs' = fold_n_map ~f ~init:acc xs in
      acc, (x' :: xs')

  module Lazy = struct
    type 'a t =
      | Nil
      | Cons of 'a * 'a t lazy_t

    let rec map ~f = function
      | Nil -> Nil
      | Cons (hd,tl) ->
         Cons (f hd, lazy (map ~f (Lazy.force tl)))

    let rec to_strict = function
      | Nil -> []
      | Cons (hd, lazy tl) -> hd :: to_strict tl

    let rec unfold f a = match f a with
      | None -> Nil
      | Some a -> Cons (a, lazy (unfold f a))

    let rec filter_map ~f = function
      | Nil -> Nil
      | Cons (a, tl) -> match f a with
        | None -> filter_map f (Lazy.force tl)
        | Some a' -> Cons (a', lazy (filter_map f (Lazy.force tl)))
  end

  type 'a non_empty =
    | One of 'a
    | More of 'a * 'a non_empty

  module Non_empty = struct
    type 'a t = 'a non_empty

    let hd = function
      | One x | More (x,_) -> x

    let rec length n = function
      | One _ -> n + 1
      | More (_,tl) -> length (n + 1) tl
    let length l = length 0 l

    let rec rev acc = function
      | One x -> More (x, acc)
      | More (x,tl) -> rev (More (x, acc)) tl
    let rev = function
      | One _ as t -> t
      | More (x,tl) -> rev (One x) tl

    let rec rev_map f acc = function
      | One x -> More (f x, acc)
      | More (x,tl) -> rev_map f (More (f x, acc)) tl
    let rev_map ~f = function
      | One x -> One (f x)
      | More (x,tl) -> rev_map f (One (f x)) tl

    let rec map ~f = function
      | One x -> One (f x)
      | More (x,tl) -> More (f x, map ~f tl)
  end

  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: l -> last l

  let rec group_by pred group acc = function
    | [] -> List.rev acc
    | x :: xs ->
      match group with
      | (x' :: _) when pred x x' ->
        group_by pred (x :: group) acc xs
      | _ -> group_by pred [x] (group :: acc) xs

  let group_by pred xs =
    match group_by pred [] [] xs with
    | [] :: xs | xs -> xs

  (* Merge sorted lists *)
  let rec merge ~cmp l1 l2 = match l1, l2 with
    | l, [] | [], l -> l
    | (x1 :: _), (x2 :: x2s) when cmp x1 x2 > 0 ->
      x2 :: merge ~cmp l1 x2s
    | x1 :: x1s, _ ->
      x1 :: merge ~cmp x1s l2

  let rec uniq ~cmp = function
    | x1 :: (x2 :: _ as xs) when cmp x1 x2 = 0 -> uniq ~cmp xs
    | x :: xs  -> x :: uniq ~cmp xs
    | [] -> []

  let sort_uniq ~cmp l =
    uniq ~cmp (sort ~cmp l)
end

module Option = struct
  let bind opt ~f =
    match opt with
    | None -> None
    | Some x -> f x

  let map ~f = function
    | None -> None
    | Some x -> Some (f x)

  let get = function
    | None -> raise Not_found
    | Some x -> x

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
    let (>>|) x f = map  x ~f
  end

  include Infix

  let to_list = function
    | None -> []
    | Some x -> [x]

  let is_some = function
    | None -> false
    | _ -> true
end

module String = struct
  include StringLabels

  let reverse s1 =
    let len = length s1 in
    let s2  = make len 'a' in
    for i = 0 to len - 1 do
      s2.[i] <- s1.[len - i - 1]
    done ;
    s2

  let common_prefix_len s1 s2 =
    let rec aux i =
      if i >= length s1 || i >= length s2 || s1.[i] <> s2.[i] then i else
      aux (succ i)
    in
    aux 0

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

  module Set = struct
    include MoreLabels.Set.Make (struct type t = string let compare = compare end)
    let of_list l = List.fold_left ~f:(fun s elt -> add elt s) l ~init:empty
    let to_list s = fold ~f:(fun x xs -> x :: xs) s ~init:[]
  end

  module Map = struct
    include MoreLabels.Map.Make (struct type t = string let compare = compare end)
    let of_list l = List.fold_left ~f:(fun m (k,v) -> add k v m) l ~init:empty
    let to_list m = fold ~f:(fun ~key ~data xs -> (key,data) :: xs) m ~init:[]

    let keys   m = fold ~f:(fun ~key ~data:_ xs -> key  :: xs) m ~init:[]
    let values m = fold ~f:(fun ~key:_ ~data xs -> data :: xs) m ~init:[]

    let add_multiple key data t =
      let current =
        try find key t
        with Not_found -> []
      in
      let data = data :: current in
      add key data t
  end

  let mem c s =
    try ignore (String.index s c : int); true
    with Not_found -> false
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

module Either = struct
  type ('a,'b) t = L of 'a | R of 'b

  let elim f g = function
    | L a -> f a
    | R b -> g b

  let try' f =
    try R (f ())
    with exn -> L exn

  let get = function
    | L exn -> raise exn
    | R v -> v

  (* Remove ? *)
  let join = function
    | R (R _ as r) -> r
    | R (L _ as e) -> e
    | L _ as e -> e

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

  let focused = function
    | Zipper (_,_,x :: _) -> Some x
    | Zipper (_,_,_) -> None

  let of_list l = Zipper ([], 0, l)
  let insert a (Zipper (prev, pos, next)) =
    Zipper (a :: prev, succ pos, next)

  let seek n (Zipper (_,pos,_) as z) =
    shift (n - pos) z

  let change_tail next (Zipper (prev,pos,_next)) =
    Zipper (prev,pos,next)

  let rec seek_forward pred head n tail =
    match tail with
    | x :: xs when pred x -> seek_forward pred (x :: head) (n + 1) xs
    | _ -> Zipper (head, n, tail)
  let seek_forward pred (Zipper (head,n,tail)) =
    seek_forward pred head n tail

  let rec seek_backward pred head n tail =
    match head with
    | x :: xs when pred x -> seek_backward pred xs (n - 1) (x :: tail)
    | _ -> Zipper (head, n, tail)
  let seek_backward pred (Zipper (head,n,tail)) =
    seek_backward pred head n tail

  let rec select_forward pred acc tail =
    match tail with
    | x :: xs when pred x -> select_forward pred (x :: acc) xs
    | _ -> acc
  let select_forward pred (Zipper (head,n,tail)) =
    select_forward pred [] tail

  let rec select_backward pred head acc =
    match head with
    | x :: xs when pred x -> select_backward pred xs (x :: acc)
    | _ -> acc
  let select_backward pred (Zipper (head,n,tail)) =
    select_backward pred head tail
end
type 'a zipper = 'a Zipper.t = Zipper of 'a list * int * 'a list

module Lexing = struct
  include Lexing

  let move buf p =
    buf.lex_abs_pos <- (p.pos_cnum - buf.lex_curr_pos);
    buf.lex_curr_p <- p

  let from_strings ?empty ?position source refill =
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
          (match empty with None -> () | Some r -> r := !pos >= !len);
          count
        end
    in
    let buf = from_function lex_fun in
    Option.iter ~f:(move buf) position;
    buf

  (* Manipulating position *)
  let make_pos ?(pos_fname="") (pos_lnum, pos_cnum) =
    { pos_fname ; pos_lnum ; pos_cnum ; pos_bol = 0 }

  let column pos = pos.pos_cnum - pos.pos_bol

  let set_column pos col = {pos with pos_cnum = pos.pos_bol + col}

  let split_pos pos = (pos.pos_lnum, column pos)

  let compare_pos p1 p2 =
    match compare p1.pos_lnum p2.pos_lnum with
    | 0 -> compare (column p1) (column p2)
    | n -> n

  let print_position ppf p =
    let line, col = split_pos p in
    Format.fprintf ppf "%d:%d" line col

  let wrap_lexer ~tokens f buf =
    match !tokens with
    | Zipper (_, _, ((t,s,c) :: _)) ->
      buf.Lexing.lex_start_p <- s;
      buf.Lexing.lex_curr_p <- c;
      tokens := Zipper.shift 1 !tokens;
      t
    | Zipper (_, _, []) ->
      let t = f buf in
      tokens := Zipper.insert Lexing.(t, buf.lex_start_p, buf.lex_curr_p) !tokens;
      t

  let const_lexer (token : 'a) (_ : lexbuf) : 'a = token

  (* Current position in lexer, even if the buffer is in the middle of a refill
     operation *)
  let immediate_pos buf =
    {buf.lex_curr_p with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos}

  let json_of_position pos =
    let line, col = split_pos pos in
    `Assoc ["line", `Int line; "col", `Int col]
end

module Stream = struct
  include Stream
  let map ~f s =
    from (fun _ ->
      try Some (f (next s))
      with Failure -> None)
end

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

module Array = struct
  include Array

  let memoize ?check n ~f =
    let f i = lazy (f i) in
    let cache = Array.init n f in
    match check with
    | None -> (fun i -> Lazy.force cache.(i))
    | Some check ->
      (fun i ->
        let cell = Lazy.force cache.(i) in
        match check cell with
        | None -> cell
        | Some cell' ->
          cache.(i) <- lazy cell';
          cell')
end

module Char = struct
  include Char
  let is_lowercase c = lowercase c = c
  let is_uppercase c = uppercase c = c
  let is_strictly_lowercase c = not (is_uppercase c)
  let is_strictly_uppercase c = not (is_lowercase c)
end

module Glob : sig
  type inst =
    | Exact of string
    | Joker
    | Skip of int
  type pattern = inst list

  val compile_pattern : string -> pattern
  val match_pattern : string -> pattern -> bool
end = struct
  type inst =
    | Exact of string
    | Joker
    | Skip of int
  type pattern = inst list

  let compile_pattern = function
    | "**" -> [Joker;Joker]
    | s ->
    let l = String.length s in
    let rec dispatch acc i =
      if i < l then match s.[i] with
      | '*' -> joker acc (i + 1)
      | '?' -> skip acc 1 (i + 1)
      | c -> string acc i (i + 1)
      else acc
    and joker acc i =
      if i < l && s.[i] = '*'
      then joker acc (i + 1)
      else dispatch (Joker :: acc) i
    and skip acc n i =
      if i < l && s.[i] = '?'
      then skip acc (n + 1) (i + 1)
      else dispatch (Skip n :: acc) (i + 1)
    and string acc i0 i =
      let valid = i < l && let c = s.[i] in c <> '*' && c <> '?' in
      if valid
      then string acc i0 (i + 1)
      else dispatch (Exact (String.sub s ~pos:i0 ~len:(i - i0)) :: acc) i
    in
    let parts = dispatch [] 0 in
    let normalize xs x =
      match x, xs with
      | Joker, (Joker :: _) | Skip 0, _ | Exact "", _ -> xs
      | Joker, ((Skip _ as skip) :: xs) -> skip :: Joker :: xs
      | Skip n, (Skip m :: xs) -> Skip (n + m) :: xs
      | Exact s, (Exact t :: xs) -> Exact (s ^ t) :: xs
      | _ -> x :: xs
    in
    List.fold_left ~f:normalize ~init:[] parts

  let match_pattern s = function
    | [Joker] -> true
    | [Exact s'] -> s = s'
    | pattern ->
      let l = String.length s in
      let exact_string i s' =
        i < l &&
        let l' = String.length s' in
        i + l' <= l &&
        let rec aux j = if j < l'
          then s.[i + j] = s'.[j] && aux (j + 1)
          else true
        in
        aux i
      in
      let rec exact_match i = function
        | Exact s' :: xs when exact_string i s' ->
          exact_match (i + String.length s') xs
        | Skip n :: xs when i + n <= l ->
          exact_match (i + n) xs
        | Exact _ :: _ | Skip _ :: _ -> None
        | (Joker :: _ | []) as xs -> Some (i, xs)
      in
      let rec joker i = function
        | Joker :: xs -> joker i xs
        | Skip n :: xs when i + n < l -> joker (i + n) xs
        | Skip _ :: _ -> false
        | [] -> true
        | (Exact s' :: _) as xs ->
          let c = s'.[0] in
          let rec aux i =
            match
              try Some (String.index_from s i c)
              with Not_found -> None
            with
            | None -> false
            | Some i -> match exact_match i xs with
              | None -> aux (i + 1)
              | Some (i, xs) -> joker i xs
          in
          aux i
      in
      match pattern with
      | Exact _ :: xs -> begin match exact_match 0 xs with
        | None -> false
        | Some (i, xs) -> joker i xs
        end
      |  xs -> joker 0 xs
end

module Obj = struct
  include Obj
  let unfolded_physical_equality a b =
    let a, b = Obj.repr a, Obj.repr b in
    if Obj.is_int a || Obj.is_int b then
      a == b
    else
      let sa, sb = Obj.size a, Obj.size b in
      sa = sb &&
      try
        for i = 0 to sa - 1 do
          if not (Obj.field a i == Obj.field b i) then
            raise Not_found
        done;
        true
      with Not_found -> false
end

let (@@) f x = f x
