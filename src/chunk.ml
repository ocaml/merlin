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

open Misc

let eof_lexer _ = Chunk_parser.EOF
let fail_lexer _ = failwith "lexer ended"
let fallback_lexer = eof_lexer

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

type 'a binding = string Location.loc * 'a Location.loc

module Context = struct
  type state = exn list * string Location.loc list (* Local modules *)

  type sig_item = Parsetree.signature_item Location.loc list or_exn
  type str_item = Parsetree.structure_item Location.loc list or_exn
  type sig_in_sig_modtype = Parsetree.modtype_declaration binding or_exn
  type sig_in_sig_module  = Parsetree.module_type binding or_exn
  type sig_in_str_modtype = Parsetree.module_type binding or_exn
  type str_in_module      = Parsetree.module_expr binding or_exn
end

let protect_parser f =
  let local_modules = ref [] in
  let exns, result =
    Misc.fluid'let 
      Outline_utils.local_modules (Some local_modules) 
      (fun () -> Location.catch_warnings f)
  in
  let exns = match result with
    | Inl exn -> exn :: exns
    | Inr _ -> exns
  in
  exns, !local_modules, result 

module Fold = struct
  (* Initial state *)
  let sig_root _ = [], []
  let str_root _ = [], []

  (* Fold items *)
  let sig_item _ = failwith "TODO"

  let str_item step (exns,modules) =
    let tokens = Outline.Spine.value step in
    let buf = Lexing.from_string "" in
    let exns', modules', result =
      protect_parser (fun () -> try
        (* run structure_item parser on tokens, appending EOF *)
        let lexer = Fake_lexer.wrap ~tokens:(ref (Zipper.of_list tokens))
            (fake_tokens [Chunk_parser.EOF, 0] fallback_lexer)
        in
        let lexer = Chunk_parser_utils.dump_lexer ~who:"chunk" lexer in
        let defs = Chunk_parser.top_structure_item lexer buf in
        defs
      with Chunk_parser.Error ->
        let loc = {Location.  
                      loc_start = buf.Lexing.lex_start_p;
                      loc_end = buf.Lexing.lex_curr_p;
                      loc_ghost = false
                   } in
        raise Syntaxerr.(Error (Other loc)))
    in
    (exns' @ exns, modules' @ modules), result

  (* Fold structure shape *)
  let str_in_module step (exns,modules) =
    let exns', modules', result =
      protect_parser (fun () ->
        let tokens = Outline.Spine.value step in
        let lexer = Fake_lexer.wrap ~tokens:(ref (Zipper.of_list tokens))
          (fake_tokens [Chunk_parser.END, 3; Chunk_parser.EOF, 0] fallback_lexer)
        in
        let open Parsetree in
        let mod_str =
          List.hd (Chunk_parser.top_structure_item lexer (Lexing.from_string ""))
        in
        begin match mod_str.Location.txt with
          | { pstr_desc = (Pstr_module (s,m)) ; pstr_loc } ->
            let m = {Location. txt = m; loc = pstr_loc} in
            (s, m)
          | _ -> assert false
        end)
    in
    (exns' @ exns, modules' @ modules), result

  (* Fold signature shape *)
  let sig_in_sig_modtype _ = failwith "TODO"
  let sig_in_sig_module  _ = failwith "TODO"
  let sig_in_str_modtype _ = failwith "TODO"
end

module Spine = Spine.Transform (Context) (Outline.Spine) (Fold)
type t = Spine.t
let update = Spine.update

let exns t = fst (Spine.get_state t)
let local_modules t = snd (Spine.get_state t)
