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

type keywords = Lexer_raw.keywords

type triple = Parser_raw.token * Lexing.position * Lexing.position

type item =
  | Triple of triple
  | Comment of (string * Location.t)
  | Error of Lexer_raw.error * Location.t

type t = {
  keywords: keywords;
  source: Merlin_source.t;
  items: item list;
}

let get_tokens keywords pos text =
  let state = Lexer_raw.make keywords in
  let lexbuf = Lexing.from_string text in
  Lexing.move lexbuf pos;
  let rec aux items = function
    | Lexer_raw.Return (Parser_raw.COMMENT comment) ->
      continue (Comment comment :: items)
    | Lexer_raw.Refill k -> aux items (k ())
    | Lexer_raw.Return t ->
      let triple = (t, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
      let items = Triple triple :: items in
      if t = Parser_raw.EOF
      then items
      else continue items
    | Lexer_raw.Fail (err, loc) ->
      continue (Error (err, loc) :: items)

  and continue items =
    aux items (Lexer_raw.token state lexbuf)

  in
  continue

let make keywords source =
  let items =
    get_tokens keywords
    { Lexing.
      pos_fname = (Merlin_source.name source);
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }
    (Merlin_source.text source)
    []
  in
  { keywords; items; source }

let text_diff source0 source1 =
  let r = ref (min (String.length source0) (String.length source1)) in
  begin try
    for i = 0 to !r - 1 do
      if source0.[i] <> source1.[i] then
        (r := i; raise Exit);
    done;
    with Exit -> ()
  end;
  !r

let item_start = function
  | Triple (_,s,_) -> s
  | Comment (_, l) | Error (_, l) ->
    l.Location.loc_start

let item_end = function
  | Triple (_,_,e) -> e
  | Comment (_, l) | Error (_, l) ->
    l.Location.loc_end

let diff items source0 source1 =
  if (Merlin_source.name source0 <> Merlin_source.name source1) then
    []
  else
    let offset =
      text_diff (Merlin_source.text source0) (Merlin_source.text source1) in
    let `Logical (line, _) =
      Merlin_source.get_logical source1 (`Offset offset) in
    List.drop_while items
      ~f:(fun i -> (item_end i).Lexing.pos_lnum >= line)

let update source t =
  if source == t.source then
    t
  else if Merlin_source.compare source t.source = 0 then
    {t with source}
  else match diff t.items t.source source with
    | [] -> make t.keywords source
    | (item :: _) as items ->
      let pos = item_end item in
      let offset = pos.Lexing.pos_cnum in
      Logger.logf "Merlin_lexer" "update" "resume from %d" offset;
      let text = Merlin_source.text source in
      let text =
          String.sub text ~pos:offset ~len:(String.length text - offset) in
      let items = get_tokens t.keywords pos text items in
      { t with items; source }

let tokens t =
  List.rev_filter_map t.items
    ~f:(function Triple t -> Some t | _ -> None)

let errors t =
  List.rev_filter_map t.items
    ~f:(function Error (err, loc) -> Some (Lexer_raw.Error (err, loc))
               | _ -> None)

let comments t =
  List.rev_filter_map t.items
    ~f:(function Comment t -> Some t | _ -> None)

let compare t1 t2 =
  if t1.keywords == t2.keywords then
    Merlin_source.compare t1.source t2.source
  else
    compare t1 t2

let source t = t.source
