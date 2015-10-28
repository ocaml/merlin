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
open Raw_parser

let section = Logger.section "explain"

let extract_annots = function
  | CT_ (_,annots) -> annots
  | CN_ (_,annots) -> annots

let production_annotations prod =
  let sym, def = Query.production_definition prod in
  let head = Option.value_map ~default:[] ~f:extract_annots sym in
  let tail = List.map ~f:extract_annots def in
  head :: tail

let production_annotations =
  Array.memoize Raw_parser.Query.productions ~f:production_annotations

let item_annotations (prod,pos) =
  let annots, tail = List.split_n (succ pos) (production_annotations prod) in
  let reducible = List.exists ~f:((=) []) tail in
  let reducing_to, symbols = Query.production_definition prod in
  let expected = List.drop_n pos symbols in
  let rec follow_nullable = function
    | [] -> []
    | hd :: tl when Query.nullable hd ->
      hd :: follow_nullable tl
    | hd :: _ -> [hd] in
  let expected = follow_nullable expected in
  let reducing_to =
    if reducible || expected = [] then
      reducing_to
    else
      None in
  reducing_to, expected, List.rev annots

let same_symbol_class sym1 sym2 =
  match sym1, sym2 with
  | CT_ (t1,_), CT_ (t2,_) -> Obj.magic t1 = Obj.magic t2
  | CN_ (n1,_), CN_ (n2,_) -> Obj.magic n1 = Obj.magic n2
  | _ -> false

let item_walk symbols (prod,pos) =
  let _, def = Query.production_definition prod in
  let nexts = List.drop_n pos def in
  let rec find_next pos = function
    | hd :: _ when List.exists ~f:(same_symbol_class hd) symbols ->
      Some (prod, pos)
    | hd :: tl when Query.nullable hd -> find_next (pos + 1) tl
    | _ -> None in
  find_next (pos + 1) nexts

let itemset_walk symbols itemset =
  List.filter_map ~f:(item_walk symbols) itemset

let itemset_annotations itemset =
  let reducing_to, expected, annots =
    List.split3 (List.map ~f:item_annotations itemset) in
  let reducing_to = List.filter_map ~f:(fun x -> x) reducing_to in
  reducing_to, List.concat expected, annots

let rec merge_annotations acc = function
  | [] ->
    begin match acc with
    | last :: body -> List.rev body, last
    | _ -> assert false
    end
  | lists ->
    let hd' = function hd :: _ -> Some hd | [] -> None in
    let tl' = function _ :: [] | [] -> None | _ :: tl -> Some tl in
    let hds = List.filter_map ~f:hd' lists in
    let tls = List.filter_map ~f:tl' lists in
    let hds = List.concat hds in
    merge_annotations (hds :: acc) tls

let merge_annotations annots = merge_annotations [] annots

let lr0_annotations lr0 =
  let reducing_to, expected, annots =
    itemset_annotations (Raw_parser.Query.itemset lr0) in
  reducing_to, expected, merge_annotations annots

let lr0_annotations =
  Array.memoize Raw_parser.Query.lr0_states ~f:lr0_annotations

let rec close_expected expected reducing_to = function
  | _ when reducing_to = [] -> expected
  | List.Lazy.Cons (lr0, next) ->
    let itemset = Raw_parser.Query.itemset lr0 in
    let itemset = itemset_walk reducing_to itemset in
    if itemset = [] then expected
    else
      let reducing_to', expected', _ =
        itemset_annotations itemset in
      close_expected (expected' @ expected) reducing_to' (Lazy.force next)
  | List.Lazy.Nil -> expected

type explanation = {
  item: (string * Merlin_parser.frame) option;
  unclosed: (string * Merlin_parser.frame) option;
  expected: Raw_parser.symbol_class list;
}

let rec annotate last frame =
  let open Merlin_parser in
  let red_, exp_, annots = lr0_annotations (Frame.lr0_state frame) in
  let body, last' =
    match annots with
    | [], last' -> [last], last'
    | hd :: tl, last' -> (last @ hd) :: tl, last'
  in
  let rec concat frame annot = match frame, annot with
    | None, _ -> lazy List.Lazy.Nil
    | Some frame, (hd :: tl) ->
      let tl = concat (Frame.next frame) tl in
      let result = List.Lazy.Cons ((frame , hd), tl) in
      lazy result
    | Some frame, [] ->
      annotate last' frame
  in
  concat (Some frame) body

let annotate frame =
  Lazy.force (annotate [] frame)

let explain parser =
  let frame = Merlin_parser.stack parser in
  let item = ref None in
  let unclosed = ref None in
  let closed = ref 0 in
  let process_annot result = function
    | `Item name when !item = None -> item := Some (name, result)
    | `Close -> incr closed
    | `Unclosed _ when !closed > 0 -> decr closed
    | `Unclosed name when !unclosed = None -> unclosed := Some (name, result)
    | _ -> ()
  in
  let rec extract = function
    | List.Lazy.Cons ((frame, annots), next) ->
      List.iter ~f:(process_annot frame) annots;
      begin match !item with
      | None -> extract (Lazy.force next)
      | Some _ -> ()
      end
    | List.Lazy.Nil -> ()
  in
  extract (annotate frame);
  let expected =
    match Merlin_parser.get_lr0_states parser with
    | List.Lazy.Nil -> assert false
    | List.Lazy.Cons (lr0, (lazy next)) ->
      let reducing_to, expected, _ = lr0_annotations lr0 in
      close_expected expected reducing_to next
  in
  Logger.infojf section ~title:"syntax error explanation expected symbols"
    (fun expected ->
       let symbols = List.map ~f:Raw_parser_values.string_of_class expected in
       let symbols = List.filter_dup symbols in
       `List (List.map (fun x -> `String x) symbols))
    expected;
  { item = !item; unclosed = !unclosed; expected }

