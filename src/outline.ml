(* {{{ COPYING *(
type token = Chunk_parser.token Fake_lexer.token

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

open Misc

type token = Chunk_parser.token Fake_lexer.token

module Context = struct
  type state = exn list * Location.t

  type sig_item = token list
  type str_item = token list
  type sig_in_sig_modtype = token list
  type sig_in_sig_module  = token list
  type sig_in_str_modtype = token list
  type str_in_module      = token list
end
module Spine = Spine.Make (Context)
type t = Spine.t

let parse_with (tokens : token zipper) ~parser ~lexer ~bufpos buf =
  let Zipper (_,origin,_) = tokens in
  let tokens' = ref tokens in
  let chunk_content tokens =
    (* Drop end of history *)
    let end_of_chunk = Zipper.change_tail [] tokens in
    let Zipper (_,_,next) = Zipper.seek origin end_of_chunk in
    (* Drop beginning of history *)
    next
  in
  let lexer = Fake_lexer.wrap ~tokens:tokens' ~bufpos lexer in
  try
    let lexer = Chunk_parser_utils.dump_lexer ~who:"outline" lexer in
    let () = parser lexer buf in
    let tokens = !tokens' in
    tokens, Outline_utils.Definition, chunk_content tokens
  with
  | Outline_utils.Chunk (c,p) ->
    begin
      let rec aux = function
        | Zipper ((t,_,p') :: _,_,_) as tokens
          when Misc.compare_pos p p' < 0 ->
          aux (Zipper.shift (-1) tokens)
        | tokens -> tokens
      in
      let tokens = aux !tokens' in
      tokens, c, chunk_content tokens
    end
  | Sys.Break ->
    begin
      let tokens = !tokens' in
      Zipper.seek origin tokens,
      Outline_utils.Unterminated, []
    end
  | Outline_parser.Error ->
    begin
      let loc = match tokens with
        | Zipper ((_prev_tok, _loc_start, loc_end) :: _,_,_) ->
          {Location. loc_start = loc_end ; loc_end ; loc_ghost=false}
        | Zipper _ ->
          {Location.
            loc_start = buf.Lexing.lex_start_p;
            loc_end   = buf.Lexing.lex_curr_p;
            loc_ghost = false}
      in
      tokens' := Zipper.shift (-2) !tokens';
      let lexer' who = Chunk_parser_utils.dump_lexer ~who lexer in
      let rec aux () =
        let count = Chunk_parser_utils.re_sync (lexer' "re_sync") buf in
        tokens' := Zipper.shift (-1) !tokens';
        let Zipper (_,offset,_) = !tokens' in
        try
          for i = 1 to count do
            try ignore (parser (lexer' "checker") buf)
            with Outline_utils.Chunk _ -> ()
          done;
          offset
        with Outline_parser.Error ->
          tokens' := Zipper.seek (succ offset) !tokens';
          aux ()
      in
      let offset = aux () in
      let tokens = Zipper.seek offset !tokens' in
      tokens, Outline_utils.Syntax_error loc, chunk_content tokens
    end
  | exn -> raise exn

let parse_str ~bufpos ~exns ~location ~lexbuf zipper t =
  match Location.catch_warnings 
      (fun () -> parse_with zipper
          ~parser:Outline_parser.implementation
          ~lexer:Lexer.token
          ~bufpos lexbuf)
  with
  | exns', Inr (zipper, _, ([] | [Chunk_parser.EOF,_,_])) -> 
    zipper, t
  | exns', Inr (zipper, Outline_utils.Unterminated, tokens) -> 
    zipper, t
  | exns', Inr (zipper, (Outline_utils.Definition | 
                               Outline_utils.Syntax_error _), tokens) -> 
    zipper,
    Spine.(Str_item (str_step t (exns' @ exns, location ()) tokens))
  | exns', Inr (zipper, 
                (Outline_utils.Enter_module | Outline_utils.Leave_module),
                _) ->
    failwith "TODO, MODULES NOT HANDLED"
  | _, Inl (Failure _ as exn) ->
    raise exn
  | exns', Inl exn ->
    zipper,
    Spine.(Str_item (str_step t (exn :: exns, location ()) []))

let exns t = fst (Spine.get_state t)
let location t = snd (Spine.get_state t)

let parse ~bufpos tokens t lexbuf =
  let exns = exns t in
  Outline_utils.reset ();
  let location = 
    let loc_start = lexbuf.Lexing.lex_curr_p in
    fun () -> let loc_end = lexbuf.Lexing.lex_start_p in
              {Location. loc_start; loc_end; loc_ghost = false}
  in
  match t with
  | Spine.Sig _ -> failwith "TODO"
  | Spine.Str t_str -> 
    let Zipper (_,_,tokens), t_str' = 
      parse_str ~bufpos ~exns ~lexbuf ~location 
        (Zipper.of_list tokens) t_str
    in
    tokens, Spine.Str t_str'

let init_loc pos_fname =
  let pos = {(Misc.make_pos (1,0)) with Lexing.pos_fname} in
  {Location. loc_start = pos; loc_end = pos; loc_ghost = false}

let initial_sig fname =
  Spine.(Sig (Sig_root (initial ([], init_loc fname))))

let initial_str fname =
  Spine.(Str (Str_root (initial ([], init_loc fname))))
