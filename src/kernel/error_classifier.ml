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

open Std
module Ex = Merlin_recovery_explain

type t = {
  loc: Location.t;
  explanation: Ex.explanation;
}
exception Error of t

let loc t = t.loc

let friendly_concat = function
  | [] -> ""
  | [a] -> a
  | [a; b] -> a ^ " or " ^ b
  | hd :: tl ->
    String.concat ", " tl ^ " or " ^ hd

let classify {explanation = {Ex. item; unclosed; expected}} =
  let inside = match item with
    | None -> ""
    | Some (name, _) -> " inside " ^ name in
  let after = match unclosed with
    | None -> ""
    | Some (name, _) -> " after unclosed " ^ name in
  let friendly_name cls =
    match Raw_parser_values.friendly_name cls, cls with
    | Some name, Raw_parser.CT_ _ -> Some ("`" ^ name ^ "'")
    | Some name, Raw_parser.CN_ _ -> Some ("<" ^ name ^ ">")
    | None, _ -> None in
  let expecting = match expected with
    | [] -> ""
    | classes ->
      let names = List.filter_map ~f:friendly_name classes in
      let names = match names with
        | [] -> List.map ~f:Raw_parser_values.string_of_class expected
        | names -> names in
      let names = List.filter_dup names in
      ", expecting " ^ friendly_concat names
  in
  Printf.sprintf "Syntax error%s%s%s"
    inside after expecting

let from parser (s,token,e) =
  let loc = {Location. loc_start = s; loc_end = e; loc_ghost = false } in
  Error { loc; explanation = Ex.explain parser }
