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

type t = { words : string list; type_expr : Type_expr.t option }

let equal { words = words_a; type_expr = type_expr_a }
    { words = words_b; type_expr = type_expr_b } =
  List.equal String.equal words_a words_b
  && Option.equal Type_expr.equal type_expr_a type_expr_b

let to_string { words; type_expr } =
  let words = String.concat "; " words in
  let type_expr =
    type_expr
    |> Option.map Type_expr.to_string
    |> Option.value ~default:"<no type-query>"
  in
  "[" ^ words ^ "] " ^ type_expr

let balance_parens len str =
  let rec aux i open_parens close_parens =
    if i >= len then (open_parens, close_parens)
    else
      match str.[i] with
      | '(' -> aux (succ i) (succ open_parens) close_parens
      | ')' when open_parens > 0 -> aux (succ i) (pred open_parens) close_parens
      | ')' -> aux (succ i) open_parens (succ close_parens)
      | _ -> aux (succ i) open_parens close_parens
  in
  let o, c = aux 0 0 0 in
  let o = String.make c '(' and c = String.make o ')' in
  o ^ str ^ c

let naive_of_string str =
  str |> String.split_on_char ' '
  |> List.filter (fun s -> not (String.equal s String.empty))

let guess_type_search len str =
  len >= 1
  && (Char.equal str.[0] '\''
     || String.contains str '-' || String.contains str '(')

let from_string str =
  let len = String.length str in
  let words, type_expr =
    match String.index_opt str ':' with
    | None ->
      if guess_type_search len str then
        let str = balance_parens len str in
        ("", Type_expr.from_string str)
      else (str, None)
    | Some loc ->
      let str_name = String.sub str 0 loc
      and str_type = String.sub str (succ loc) (len - loc - 1) in
      let len = String.length str_type in
      let str_type = balance_parens len str_type in
      (str_name, Type_expr.from_string str_type)
  in
  let words = naive_of_string words in
  { words; type_expr }

let distance_for { words; type_expr } ~path candidate =
  let type_cost =
    type_expr
    |> Option.map (fun query -> Type_distance.compute ~query ~entry:candidate)
    |> Option.value ~default:0
  in
  let name_cost = Name_cost.best_distance words path in
  name_cost + type_cost
