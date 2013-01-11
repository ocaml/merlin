type item_desc =
  | Root
  | Definition of Parsetree.structure_item * item_desc
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr * item_desc

type item = Outline.Chunked.sync * item_desc
type sync = item History.sync
type t = item History.t
type directive = string * Parsetree.directive_argument

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

let append_step chunk tokens t =
  match chunk with
    | Outline_utils.Enter_module ->
        let lexer = History.wrap_lexer (ref (History.of_list tokens))
          (fake_tokens [Chunk_parser.END, 3; Chunk_parser.EOF, 0] fail_lexer)
        in
        (* let lexer = Chunk_parser_utils.print_tokens lexer in *)
        let open Parsetree in
        begin match Chunk_parser.top_structure_item lexer (Lexing.from_string "") with
          | { pstr_desc = (Pstr_module (s,m)) ; pstr_loc } ->
              None, Module_opening (pstr_loc, s, m, t)
          | _ -> assert false
        end
        (* run structure_item parser on tokens, appending END EOF *)
    | Outline_utils.Leave_module ->
        (* reconstitute module from t *)
        let rec gather_defs defs = function
          | Root -> raise Malformed_module
          | Definition (d,t) -> gather_defs (d :: defs) t
          | Module_opening (loc,s,m,t) ->
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
              Definition ({ pstr_desc = Pstr_module (s, subst_structure m);
                            pstr_loc  = loc },
                          t)
        in
        None, gather_defs [] t
    | Outline_utils.Definition ->
        (* run structure_item parser on tokens, appending EOF *)
        let lexer = History.wrap_lexer (ref (History.of_list tokens))
          (fake_tokens [Chunk_parser.EOF, 0] fail_lexer)
        in
        (* let lexer = Chunk_parser_utils.print_tokens lexer in *)
        None, Definition (Chunk_parser.top_structure_item lexer (Lexing.from_string ""), t)
    | Outline_utils.Done | Outline_utils.Unterminated | Outline_utils.Exception _ -> None, t
    | Outline_utils.Rollback -> raise Invalid_chunk
    | Outline_utils.Directive ->
        let lexer = History.wrap_lexer (ref (History.of_list tokens))
          (fake_tokens [Chunk_parser.EOF, 0] fail_lexer)
        in
        Some (Chunk_parser.top_directive lexer (Lexing.from_string "")), t

let append chunks history =
  (* Find last synchronisation point *)
  print_endline "SYNC PARSER";
  let chunks, history = History.sync fst chunks history in
  (* Drop out of sync items *)
  let history, out_of_sync = History.split history in
  (* Process last items *) 
  let rec aux chunks history item =
    match History.forward chunks with
      | None -> history, None, item
      | Some ((_,(filter,chunk,data,exns)),chunks') ->
          match append_step chunk data item with
            | (Some _ as directive), item -> history, directive, item
            | None, item ->
                let history = History.insert (History.sync_point chunks', item) history in
                aux chunks' history item
  in
  let history, directive, item = aux chunks history Root in
  directive, history
