type token = Chunk_parser.token History.loc

exception Parse_error of Location.t

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
    let lexer = Chunk_parser_utils.print_tokens ~who:"outline" lexer in
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
          let lexer' who = Chunk_parser_utils.print_tokens ~who lexer in
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
    | exn ->
        history, Outline_utils.Exception exn, []

type item = {
  kind       : Outline_utils.kind;
  tokens     : token list;
  exns : exn list;
}
type sync = item History.sync
type t = item History.t

let last_curr = List.fold_left (fun _ (_,_,curr) -> curr)

let item_start = function
  | { tokens = ((_,loc_start,_) :: _) } -> loc_start
  | _ -> failwith "Outline.item_start: Invalid item"

let start t =
  match History.prev t with
    | Some { tokens = ((_,loc_start,_) :: _) } -> Some loc_start
    | None -> None
    | _ -> failwith "Outline.start: Invalid t"

let location t =
  match History.prev t with
    | Some { tokens = ((_,loc_start,curr) :: xs) } ->
        let loc_end = last_curr curr xs in
        Location.({ loc_start ; loc_end = loc_end ; loc_ghost = false })
    | None -> Location.none
    | _ -> failwith "Outline.location: Invalid t"

let seek cmp t =
  let open Lexing in
  let seek_func { tokens } =
    match tokens with
      | (_,start,_) :: _ when cmp start < 0 -> -1
      | (_,_,curr) :: xs when cmp curr < 0 || cmp (last_curr curr xs) < 0 -> 0
      | [] -> failwith "Outline.seek: Invalid t"
      | _ -> 1
  in
  let go_forward t = seek_func t > 0 in
  let go_backward t = seek_func t < 0 in
  History.seek_backward go_backward (History.seek_forward go_forward t)

let seek_before pos t =
  let cmp = Misc.compare_pos pos in
  let t = seek cmp t in
  let rec rewind t =
    match location t with
      | l when l = Location.none -> t
      | l when cmp l.Location.loc_end > 0 -> t
      | _ -> match History.backward t with
          | Some (_,t') -> rewind t'
          | None -> t
  in
  rewind t

let seek_offset offset =
  seek (fun pos -> compare offset pos.Lexing.pos_cnum)

let parse_step ?bufpos ?(exns=[]) history buf =
  Outline_utils.reset ();
  let history', kind, tokens = parse_with history
    ~parser:Outline_parser.implementation
    ~lexer:Lexer.token
    ?bufpos buf
  in
  let exns = match kind with
    | Outline_utils.Syntax_error loc -> Parse_error loc :: exns
    | Outline_utils.Exception exn -> exn :: exns
    | _ -> exns
  in
  history',
  (match tokens with
    | [] -> None
    | _ -> Some { kind ; tokens ; exns })

let exns chunks =
  match History.prev chunks with
    | Some { exns } -> exns
    | None -> []

let append_exns exns =
  History.modify (fun o -> { o with exns = exns @ o.exns })

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
    | tokens', Some { kind = Outline_utils.Unterminated } ->
        tokens', chunks
    | tokens', Some item ->
        tokens', History.insert item chunks
    | tokens', None ->
        tokens', chunks

let parse ?bufpos tokens chunks buf =
  let tokens, chunks = parse ?bufpos tokens chunks buf in
  History.nexts tokens, chunks

