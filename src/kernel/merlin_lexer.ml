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

type t = {
  keywords: keywords;
  tokens: (Parser_raw.token * Lexing.position * Lexing.position) list;
  errors: exn list;
  comments: (string * Location.t) list;
  source: Merlin_source.t;
}

let get_tokens keywords source =
  let state = Lexer_raw.make keywords in
  let lexbuf = Lexing.from_string (Merlin_source.text source) in
  Lexing.move lexbuf
    { Lexing.
      pos_fname = (Merlin_source.name source);
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };
  let rec aux items errors comments = function
    | Lexer_raw.Return Parser_raw.EOF ->
      List.rev items, List.rev errors, List.rev comments
    | Lexer_raw.Return (Parser_raw.COMMENT comment) ->
      continue items errors (comment :: comments)
    | Lexer_raw.Refill k -> aux items errors comments (k ())
    | Lexer_raw.Return t ->
      let item = (t, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p) in
      continue (item :: items) errors comments
    | Lexer_raw.Fail (err, loc) ->
      continue items (Lexer_raw.Error (err, loc) :: errors) comments

  and continue items errors comments =
    aux items errors comments (Lexer_raw.token state lexbuf)

  in
  continue [] [] []

let make keywords source =
  let tokens, errors, comments = get_tokens keywords source in
  { keywords; tokens; errors; comments; source }

let update source t =
  if source == t.source then
    t
  else if Merlin_source.compare source t.source = 0 then
    {t with source}
  else
    make t.keywords source

let tokens t = t.tokens
let errors t = t.errors
let comments t = t.comments

let compare t1 t2 =
  if t1.keywords == t2.keywords then
    Merlin_source.compare t1.source t2.source
  else
    compare t1 t2

let source t = t.source
