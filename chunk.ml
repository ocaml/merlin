type item_desc =
  | Definitions of Parsetree.structure_item Location.loc list
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr
  | Module_closing of Parsetree.structure_item Location.loc * History.offset
  | Partial_definitions of Parsetree.structure_item Location.loc list

type item = Outline.sync * item_desc
type sync = item History.sync
type t = item History.t

exception Malformed_module
exception Invalid_chunk

let eof_lexer _ = Chunk_parser.EOF
let fail_lexer _ = failwith "lexer ended"
let fallback_lexer = eof_lexer

let line x = (x.Location.loc.Location.loc_start.Lexing.pos_lnum)

let dump_chunk t =
  List.map
  begin function
  | _, Definitions [] -> assert false
  | _, Definitions (d :: _) -> ("definition", line d)
  | _, Module_opening (l,s,_) -> ("opening " ^ s.Location.txt, line s)
  | _, Module_closing (d,offset) -> ("closing after " ^ string_of_int offset, line d)
  | _, Partial_definitions [] -> ("empty partial_definitions", -1)
  | _, Partial_definitions (d :: _) -> ("partial_definitions", line d)
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
            Some (Module_opening (pstr_loc, s, m))
        | _ -> assert false
        end
    | Outline_utils.Definition ->
        (* run structure_item parser on tokens, appending EOF *)
        let lexer = History.wrap_lexer (ref (History.of_list tokens))
          (fake_tokens [Chunk_parser.EOF, 0] fallback_lexer)
        in
        let lexer = Chunk_parser_utils.print_tokens ~who:"chunk" lexer in
        let defs = Chunk_parser.top_structure_item lexer (Lexing.from_string "") in
        Some (Definitions defs)

    | Outline_utils.Done | Outline_utils.Unterminated | Outline_utils.Exception _ -> None
    | Outline_utils.Rollback -> raise Invalid_chunk

    | Outline_utils.Leave_module ->
        (* reconstitute module from t *)
        let rec rewind_defs defs t =
          match History.backward t with
          | Some ((_, Definitions []), _) -> assert false
          | Some ((_,Definitions (d::_)), t') -> rewind_defs (d.Location.txt :: defs) t'
          | Some ((_,Partial_definitions _), t') -> rewind_defs defs t'
          | Some ((_,Module_closing (d,offset)), t') ->
              rewind_defs (d.Location.txt :: defs) (History.seek_offset offset t')
          | Some ((_,Module_opening (loc,s,m)), t') -> loc,s,m,defs,t'
          | None -> raise Malformed_module
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
        Some (Module_closing (
                Location.mkloc {
                  pstr_desc = Pstr_module (s, subst_structure m);
                  pstr_loc  = loc
                } loc,
                History.offset t
             ))
    | Outline_utils.Partial_definitions defs ->
        let rec list_drop n = function
          | x :: xs when n > 0 -> list_drop (pred n) xs
          | xs -> xs
        in
        let list_split_n n l =
          let rec aux n acc = function
            | x :: xs when n > 0 -> aux (pred n) (x :: acc) xs
            | xs -> acc, xs
          in
          let acc, rest = aux n [] l in
          (List.rev acc), rest
        in
        let rec extract offset defs tokens =
          match defs, tokens with
            | _, [] -> []
            | (starto,endo) :: defs, _ ->
                let tokens = list_drop (starto - offset -1) tokens in
                let def, tokens = list_split_n (endo - starto + 1) tokens in
                def :: extract endo defs tokens
            | _ -> []
        in
        let defs = extract 0 defs tokens in
        let parse_def tokens =
          let lexer = History.wrap_lexer (ref (History.of_list tokens))
            (fake_tokens [Chunk_parser.EOF, 0] fallback_lexer)
          in
          let lexer = Chunk_parser_utils.print_tokens ~who:"chunk:partial" lexer in
          match Chunk_parser.top_structure_item lexer (Lexing.from_string "") with
          | [def] -> def
          | _ -> assert false (* we're never seing type definitions here *)
        in
        Some (Partial_definitions (List.map parse_def defs))

let sync outlines chunks =
  (* Find last synchronisation point *)
  let outlines, chunks = History.Sync.rewind fst outlines chunks in
  (* Drop out of sync items *)
  let chunks = History.cutoff chunks in
  (* Process last items *)
  let rec aux outlines chunks =
    match History.forward outlines with
      | None -> chunks
      | Some ({ Outline.kind ; Outline.tokens },outlines') ->
          (*prerr_endline "SYNC PARSER";*)
          match
            try
              match sync_step kind tokens chunks with
                | Some chunk -> Some (History.insert (History.Sync.at outlines', chunk) chunks)
                | None -> None
            with Syntaxerr.Error _ -> None
          with
            | Some chunks -> aux outlines' chunks
            | None -> aux outlines' chunks
  in
  aux outlines chunks

