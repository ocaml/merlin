type t =
  | Root
  | Definition of Parsetree.structure_item * t
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr * t

exception Malformed_module
exception Invalid_chunk

let empty = Root

let eof_lexer _ = Chunk_parser.EOF
let fail_lexer _ = failwith "lexer ended"

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

let print_toks f a =
  let t = f a in
  print_endline (Outline.token_to_string t);
  t

let append chunk tokens t =
  match chunk with
    | Outline_utils.Enter_module ->
        let lexer = History.wrap (ref tokens)
          (fake_tokens [Chunk_parser.END, 3; Chunk_parser.EOF, 0] fail_lexer)
        in
        let lexer = print_toks lexer in
        let open Parsetree in
        begin match Chunk_parser.top_structure_item lexer (Lexing.from_string "") with
          | { pstr_desc = (Pstr_module (s,m)) ; pstr_loc } ->
              Module_opening (pstr_loc, s, m, t)
          | _ -> assert false
        end
        (* run structure_item parser on tokens, appending END EOF *)
    | Outline_utils.Leave_module ->
        (* reconstitute module from t *)
        let rec gather_defs defs = function
          | Root -> raise Malformed_module
          | Definition (d,t) -> gather_defs (d :: defs) t
          | Module_opening (start,s,m,t) ->
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
              Definition ({ pstr_desc = Pstr_module (s, subst_structure m);
                            pstr_loc  = { start with Location.loc_end = History.last_pos tokens } },
                          t)
        in
        gather_defs [] t
    | Outline_utils.Definition ->
        (* run structure_item parser on tokens, appending EOF *)
        let lexer = History.wrap (ref tokens)
          (fake_tokens [Chunk_parser.EOF, 0] fail_lexer)
        in
        let lexer = print_toks lexer in
        Definition (Chunk_parser.top_structure_item lexer (Lexing.from_string ""), t)
    | Outline_utils.Done | Outline_utils.Unterminated ->
       t
    | _ -> raise Invalid_chunk

let append_history chunk data : Outline_utils.chunk -> Chunk_parser.token History.t -> t History.t -> t History.t 
