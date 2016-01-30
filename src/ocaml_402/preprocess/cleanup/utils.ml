open MenhirSdk
open Cmly_format

let const c = fun _ -> c

let group_assoc l =
  let cons k v acc = (k, List.rev v) :: acc in
  let rec aux k v acc = function
    | [] -> List.rev (cons k v acc)
    | (k', v') :: xs when compare k k' = 0 ->
        aux k (v' :: v) acc xs
    | (k', v') :: xs ->
        aux k' [v'] (cons k v acc) xs
  in
  match List.sort compare l with
  | [] -> []
  | (k, v) :: xs -> aux k [v] [] xs

(* negation to put nan as the max *)
let compare_float a b = - compare (-.a) (-.b)

let min_float a b =
  if compare_float a b > 0 then b else a

let arg_min_float f a b =
  if compare_float (f a) (f b) <= 0 then a else b

exception Found of int
let array_exists arr f =
  try
    for i = 0 to Array.length arr - 1 do
      if f arr.(i) then raise (Found i);
    done;
    false
  with Found _ -> true

let array_findi arr f =
  match
    for i = 0 to Array.length arr - 1 do
      if f arr.(i) then raise (Found i);
    done
  with () -> raise Not_found
     | exception (Found i) -> i

let array_find arr f =
  arr.(array_findi arr f)

let array_assoc arr x =
  snd (array_find arr (fun (x',_) -> compare x x' = 0))

let list_fmt f l =
  "[" ^ String.concat "; " (List.map f l) ^ "]"

let fst3 (x,_,_) = x

let is_attribute name (name', stretch : attribute) =
  name = Positions.value name'

let string_of_stretch s =
  s.Stretch.stretch_raw_content

let string_of_type = function
  | Stretch.Inferred s -> s
  | Stretch.Declared s -> string_of_stretch s

let name_of_symbol = function
  | T t -> t.t_name
  | N n -> n.n_name

let items_table ?(annots=[]) items =
  let last_lhs = ref (-1) in
  let prepare (p,pos) annot =
    let rhs = Array.map (fun (sym, id, _) ->
        if id <> "" && id.[0] <> '_' then
          "(" ^ id ^ " = " ^ name_of_symbol sym ^ ")"
        else name_of_symbol sym)
        p.p_rhs
    in
    if pos >= 0 && pos < Array.length rhs then
      rhs.(pos) <- ". " ^ rhs.(pos)
    else if pos > 0 && pos = Array.length rhs then
      rhs.(pos - 1) <- rhs.(pos - 1) ^ " .";
    let rhs = Array.to_list rhs in
    let rhs =
      if !last_lhs = p.p_lhs.n_index then
        "" :: "  |" :: rhs
      else
        (last_lhs := p.p_lhs.n_index;
         p.p_lhs.n_name :: "::=" :: rhs)
    and annot =
      "" :: "" :: annot
    in
    [rhs; annot]
  in
  let annots =
    annots @
    Array.to_list (Array.make (List.length items - List.length annots) [])
  in
  List.concat (List.map2 prepare items annots)

let print_table ppf ?(prefix="") ?(sep=" ") table =
  let align_tabular rows =
    let rec lengths l acc = function
      | [] when l = -1 -> []
      | [] ->
          l :: lengths (-1) [] acc
      | [] :: rows ->
          lengths l acc rows
      | (col :: cols) :: rows ->
          lengths (max l (String.length col)) (cols :: acc) rows
    in
    let lengths = lengths (-1) [] rows in
    let rec adjust_length lengths cols = match lengths, cols with
      | l :: ls, c :: cs ->
          let pad = l - String.length c in
          let c =
            if pad = 0 then c
            else c ^ String.make pad ' '
          in
          c :: adjust_length ls cs
      | _, [] -> []
      | [], _ -> assert false
    in
    List.map (adjust_length lengths) rows
  in
  let table = align_tabular table in
  List.iter
    (fun line -> Format.fprintf ppf "%s%s\n" prefix (String.concat sep line))
    table
