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

type item_desc =
  | Definitions of Parsetree.structure_item Location.loc list
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr
  | Module_closing of Parsetree.structure_item Location.loc * History.offset

type step = (Outline_utils.kind, item_desc) Misc.sum
type item = Outline.sync * (exn list * step)
type sync = item History.sync
type t = item History.t

exception Malformed_module of Location.t
exception Invalid_chunk

let eof_lexer _ = Chunk_parser.EOF
let fail_lexer _ = failwith "lexer ended"
let fallback_lexer = eof_lexer

let line x = (x.Location.loc.Location.loc_start.Lexing.pos_lnum)

let dump_chunk t =
  let open Misc in
  List.map
  begin function
  | _, (_, Inr (Definitions [])) -> assert false
  | _, (_, Inr (Definitions (d :: _))) -> ("definition", line d)
  | _, (_, Inr (Module_opening (l,s,_))) -> ("opening " ^ s.Location.txt, line s)
  | _, (_, Inr (Module_closing (d,offset))) -> ("closing after " ^ string_of_int offset, line d)
  | _, (_, Inl _) -> ("other", -1)
  end (List.rev (History.prevs t) @ History.nexts t)

let fake_tokens tokens f =
  let tokens = ref tokens in
  fun lexbuf ->
    match !tokens with
      | (t, sz) :: ts ->
          let open Lexing in
          lexbuf.lex_start_p <- lexbuf.lex_curr_p;
          lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum + sz };
          tokens := ts;
          t
      | _ -> f lexbuf

let sync_step outline tokens t =
  match outline with
  | Outline_utils.Enter_module ->
    let lexer = History.wrap_lexer (ref (History.of_list tokens))
        (fake_tokens [Chunk_parser.END, 3; Chunk_parser.EOF, 0] fallback_lexer)
    in
    let open Parsetree in
    let mod_str =
      List.hd (Chunk_parser.top_structure_item lexer (Lexing.from_string ""))
    in
    begin match mod_str.Location.txt with
      | { pstr_desc = (Pstr_module (s,m)) ; pstr_loc } ->
        Misc.Inr (Module_opening (pstr_loc, s, m))
      | _ -> assert false
    end
  | Outline_utils.Definition | Outline_utils.Syntax_error _ ->
    let buf = Lexing.from_string "" in
    begin try
        (* run structure_item parser on tokens, appending EOF *)
        let lexer = History.wrap_lexer (ref (History.of_list tokens))
            (fake_tokens [Chunk_parser.EOF, 0] fallback_lexer)
        in
        let lexer = Chunk_parser_utils.dump_lexer ~who:"chunk" lexer in
        let defs = Chunk_parser.top_structure_item lexer buf in
        Misc.Inr (Definitions defs)
      with Chunk_parser.Error ->
        raise Syntaxerr.(
            Error (Other { Location.  
                           loc_start = buf.Lexing.lex_start_p;
                           loc_end = buf.Lexing.lex_curr_p;
                           loc_ghost = false
                         }))
    end

  | Outline_utils.Done
  | Outline_utils.Unterminated
  | Outline_utils.Rollback -> 
    Misc.Inl outline

  (* Can now occurs here when a malformed module contains a definition that should have been
   * rolled back but cannot, as in:
   *   exception E
   *   and X
  *)

  | Outline_utils.Leave_module ->
    (* reconstitute module from t *)
    let rec rewind_defs defs t =
      match History.backward t with
      | Some ((_,(_,Misc.Inr (Definitions []))), _) -> assert false
      | Some ((_,(_,Misc.Inr (Definitions lst))), t') -> 
        rewind_defs (List.map (fun d -> d.Location.txt) lst @ defs) t'
      | Some ((_,(_,Misc.Inr (Module_closing (d,offset)))), t') ->
        rewind_defs (d.Location.txt :: defs) (History.seek_offset offset t')
      | Some ((_,(_,Misc.Inr (Module_opening (loc,s,m)))), t') -> loc,s,m,defs,t'
      | Some ((_,(_,Misc.Inl _)), t') -> rewind_defs defs t'
      | None ->
        let p = (match tokens with (_,loc_start,loc_end) :: _ -> Location.({loc_start;loc_end;loc_ghost = false}) | _ -> Location.none) in
        raise (Malformed_module p)
    in
    let loc,s,m,defs,t = rewind_defs [] t in
    let open Parsetree in
    let rec subst_structure e =
      let pmod_desc = match e.pmod_desc with
        | Pmod_structure _ ->
          Pmod_structure defs
        | Pmod_functor (s,t,e) ->
          Pmod_functor (s,t,subst_structure e)
        | Pmod_constraint (e,t) ->
          Pmod_constraint (subst_structure e, t)
        | Pmod_apply  _ | Pmod_unpack _ | Pmod_ident  _ -> assert false
      in
      { e with pmod_desc }
    in
    let loc = match tokens with
      | (_,_,p) :: _ -> { loc with Location.loc_end = p }
      | [] -> loc
    in
    Misc.Inr (Module_closing (
        Location.mkloc {
          pstr_desc = Pstr_module (s, subst_structure m);
          pstr_loc  = loc
        } loc,
        History.offset t
      ))

let exns h = match History.prev h with
  | Some (_, (exns,_)) -> exns
  | None -> []

let sync outlines chunks =
  (* Find last synchronisation point *)
  let outlines, chunks = History.Sync.rewind fst outlines chunks in
  (* Drop out of sync items *)
  let chunks = History.cutoff chunks in
  (* Process last items *)
  let rec aux outlines chunks =
    match History.forward outlines with
      | None -> chunks
      | Some ({ Outline. kind ; tokens ; loc },outlines') ->
          let exns = exns chunks in
          let chunk =
            match Merlin_parsing.catch_warnings (fun () -> sync_step kind tokens chunks) with
            | warnings, Misc.Inr item ->
              warnings @ exns , item
            | warnings, Misc.Inl exn ->
              exn :: warnings @ exns, Misc.Inl (Outline_utils.Syntax_error loc)
          in
          let chunks' = History.(insert (Sync.at outlines', chunk) chunks) in
          aux outlines' chunks'
  in
  aux outlines chunks
