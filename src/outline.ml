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

type token = Chunk_parser.token History.loc

let parse_with history ~parser ~lexer ?bufpos buf =
  let origin = History.current_pos history in
  let history' = ref history in
  let chunk_content h =
    (* Drop end of history *)
    let end_of_chunk = History.cutoff h in
    let at_origin = History.seek_pos origin end_of_chunk in
    (* Drop beginning of history *)
    History.nexts at_origin
  in
  let lexer = History.wrap_lexer ?bufpos history' lexer in
  try
    let lexer = Chunk_parser_utils.dump_lexer ~who:"outline" lexer in
    let () = parser lexer buf in
    let history = !history' in
    history, Outline_utils.Done, chunk_content history
  with
  | Outline_utils.Chunk (c,p) ->
    begin
      let history = !history' in
      let history = match History.backward history with
        | Some ((t,_,p'), history) when Lexing.(p.pos_cnum < p'.pos_cnum) ->
          history
        | _ -> history
      in
      history, c, chunk_content history
    end
  | Sys.Break ->
    begin
      let history = !history' in
      History.(seek_pos origin history),
      Outline_utils.Unterminated,
      []
    end
  | Outline_parser.Error ->
    begin
      let loc = match History.prev history with
        | Some (_prev_tok, _loc_start, loc_end) ->
          Location.({ loc_start = loc_end ; loc_end ; loc_ghost=false })
        | None ->
          Location.({
            loc_start = buf.Lexing.lex_start_p ;
            loc_end   = buf.Lexing.lex_curr_p ;
            loc_ghost = false ;
          })
      in
      history' := History.move (-1) !history';
      let lexer' who = Chunk_parser_utils.dump_lexer ~who lexer in
      let rec aux () =
        let count = Chunk_parser_utils.re_sync (lexer' "re_sync") buf in
        history' := History.move (-1) !history';
        let offset = History.offset !history' in
        try
          for i = 1 to count do
            try ignore (parser (lexer' "checker") buf)
            with Outline_utils.Chunk _ -> ()
          done;
          offset
        with Outline_parser.Error ->
          history' := History.seek_offset (succ offset) !history';
          aux ()
      in
      let offset = aux () in
      let history =
        History.seek_offset offset !history'
      in
      history, Outline_utils.Syntax_error loc, chunk_content history
    end
  | exn -> raise exn

type item = {
  kind       : Outline_utils.kind;
  loc        : Location.t;
  tokens     : token list;
  exns       : exn list;
}
type sync = item History.sync
type t = item History.t

let item_loc i = i.loc

let location t =
  match History.prev t with
  | Some i -> i.loc
  | None -> Location.none

let parse_step ?bufpos ?(exns=[]) history buf =
  Outline_utils.reset ();
  let location = 
    let loc_start = buf.Lexing.lex_curr_p in
    function
    | []  ->
      let loc_end = buf.Lexing.lex_start_p in
      { Location. loc_start ; loc_end ; loc_ghost = false }
    | (_, loc_start, curr) :: xs ->
      let loc_end = List.fold_left (fun _ -> Misc.thd3) curr xs in
      { Location. loc_start ; loc_end ; loc_ghost = false }
  in
  let exns', history', kind, tokens = 
    match Merlin_parsing.catch_warnings 
        (fun () -> parse_with history
            ~parser:Outline_parser.implementation
            ~lexer:Lexer.token
            ?bufpos buf)
    with
    | exns', Misc.Inr (history', kind, tokens) -> 
      exns', history', kind, tokens
    | _, Misc.Inl (Failure _ as exn) -> raise exn
    | exns', Misc.Inl exn ->
      exn :: exns', history,
      Outline_utils.Syntax_error (location []),
      []
  in
  history',
  (match tokens, exns' with
   | [], [] -> None
   | _ -> Some { kind ; loc = location tokens; tokens ; exns = exns' @ exns })

let exns chunks =
  match History.prev chunks with
  | Some { exns } -> exns
  | None -> []

let append_exns exns outlines = match History.prev outlines with
  | None -> 
    History.insert {
      kind = Outline_utils.Syntax_error Location.none;
      tokens = [];
      loc = Location.none;
      exns;
    } outlines
  | Some _ -> History.modify (fun o -> { o with exns = exns @ o.exns }) outlines

let rec do_rollback next_tokens chunks =
  match History.backward chunks with
  | Some ({ tokens ; kind = Outline_utils.Rollback }, chunks') ->
    do_rollback (tokens @ next_tokens) chunks'
  | None -> next_tokens, chunks
  | Some ({ tokens }, chunks') -> tokens @ next_tokens, chunks'

let rec parse ?(can_rollback=true) ?bufpos tokens chunks buf =
  let exns = exns chunks in
  match parse_step ?bufpos ~exns (History.of_list tokens) buf with
  | tokens', Some { kind = Outline_utils.Rollback } when can_rollback ->
    let tokens = History.nexts (History.seek_offset 0 tokens') in
    let tokens, chunks = do_rollback tokens chunks in
    let chunks = History.cutoff chunks in
    parse ~can_rollback:false ?bufpos tokens chunks buf
  | tokens', Some { kind = (Outline_utils.Unterminated | Outline_utils.Done) } ->
    tokens', chunks
  | tokens', Some item ->
    tokens', History.insert item chunks
  | tokens', None ->
    tokens', chunks

let parse ?bufpos tokens chunks buf =
  let tokens, chunks = parse ?bufpos tokens chunks buf in
  History.nexts tokens, chunks

