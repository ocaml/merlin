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

open Std
open Misc

type token = Chunk_parser.token * Lexing.position * Lexing.position

module Context = struct
  type state = exn list * Location.t * token list

  type sig_item = token list
  type str_item = token list
  type sig_in_sig_modtype = token list
  type sig_in_sig_module  = token list
  type sig_in_str_modtype = token list
  type str_in_module      = token list
end
module Spine = Spine.Initial (Context)
type t = Spine.t

let parse_with (tokens : token zipper) ~parser ~lexer buf =
  let Zipper (_,origin,_) = tokens in
  let tokens' = ref tokens in
  let chunk_content tokens =
    (* Drop end of history *)
    let end_of_chunk = Zipper.change_tail [] tokens in
    let Zipper (_,_,next) = Zipper.seek origin end_of_chunk in
    (* Drop beginning of history *)
    next
  in
  let lexer = Lexing.wrap_lexer ~tokens:tokens' lexer in
  try
    let lexer = Chunk_parser_utils.dump_lexer ~who:"outline" lexer in
    let () = parser lexer buf in
    let tokens = !tokens' in
    tokens, Outline_utils.Definition, chunk_content tokens
  with
  | Outline_utils.Chunk (c,p) ->
    begin
      let rec aux = function
        | Zipper ((_,_,p') :: _,_,_) as tokens
          when Lexing.compare_pos p p' < 0 ->
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
          for _i = 1 to count do
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

exception Malformed_module of token list * Location.t

let parse_str ~exns ~location ~lexbuf zipper t =
  let new_state exns' tokens = (exns' @ exns, location tokens, tokens) in
  match Merlin_parsing.catch_warnings
      (fun () -> parse_with zipper
          ~parser:Outline_parser.implementation
          ~lexer:Raw_lexer.token
          lexbuf)
  with
  | _exns, Either.R (zipper, _, ([] | [Chunk_parser.EOF,_,_])) ->
    zipper, None
  | _exns, Either.R (zipper, Outline_utils.Unterminated, _tokens) ->
    zipper, None
  | exns', Either.R (zipper, (Outline_utils.Definition |
                         Outline_utils.Syntax_error _), tokens) ->
    zipper,
    Some Spine.(Str_item (str_step t (new_state exns' tokens) tokens))
  | exns', Either.R (zipper, Outline_utils.Enter_module, tokens) ->
    zipper,
    Some Spine.(Str_in_module (str_step t (new_state exns' tokens) tokens))
  | _exns, Either.R (zipper, Outline_utils.Leave_module, tokens) ->
    let rec aux acc = function
      | Spine.Str_root _step ->
        let exn = Malformed_module (tokens, location tokens) in
        Spine.(Str_item (str_step t (exn :: exns, location tokens, tokens) []))
      | Spine.Str_in_module step ->
        let exns, _, _ = Spine.state step in
        let tokens' = Spine.value step @ acc in
        let parent = Spine.parent step in
        Spine.Str_item (Spine.str_step parent (exns, location tokens, tokens) tokens')
      | Spine.Str_item step ->
        let tokens = Spine.value step @ acc in
        let parent = Spine.parent step in
        aux tokens parent
    in
    zipper,
    Some (aux tokens t)
  | _, Either.L (Failure _ as exn) ->
    raise exn
  | _exns, Either.L exn ->
    zipper,
    Some Spine.(Str_item (str_step t (exn :: exns, location [], []) []))

let exns t = fst3 (Spine.get_state t)
let location t = snd3 (Spine.get_state t)
let tokens t = thd3 (Spine.get_state t)

let parse tokens t lexbuf =
  let exns = exns t in
  Outline_utils.reset ();
  let location =
    let loc_start = lexbuf.Lexing.lex_curr_p in function
    | [] -> let loc_end = lexbuf.Lexing.lex_start_p in
      {Location. loc_start; loc_end; loc_ghost = false}
    | (_,loc_start,loc_end) :: toks ->
      let loc_end = List.fold_left toks ~init:loc_end
        ~f:(fun _ (_,_,loc_end) -> loc_end)
      in
      {Location. loc_start; loc_end; loc_ghost = false}
  in
  match t with
  | Spine.Sig _ -> failwith "TODO"
  | Spine.Str t_str ->
    let Zipper (_,_,tokens), t_str' =
      parse_str ~exns ~lexbuf ~location
        (Zipper.of_list tokens) t_str
    in
    tokens, Option.map (fun x -> Spine.Str x) t_str'

let init_loc pos_fname =
  let pos = {(Lexing.make_pos (1,0)) with Lexing.pos_fname} in
  {Location. loc_start = pos; loc_end = pos; loc_ghost = false}

let initial_sig fname =
  Spine.(Sig (Sig_root (initial ([], init_loc fname, []))))

let initial_str fname =
  Spine.(Str (Str_root (initial ([], init_loc fname, []))))

let invalid = function
  | Spine.Sig (Spine.Sig_item step) -> Spine.value step = []
  | Spine.Str (Spine.Str_item step) -> Spine.value step = []
  | _ -> false
