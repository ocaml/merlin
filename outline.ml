module Raw =
struct
  type token = Chunk_parser.token
  type item = token History.loc  
  type sync = item History.sync
  type t = item History.t 
end

let parse_with history ~parser ~lexer ?bufpos buf =
  let origin = History.current_pos history in
  let history' = ref history in
  let chunk_content h =
    let open History in
    (* Drop end of history *)
    let end_of_chunk, _ = split h in
    let at_origin = seek_pos origin end_of_chunk in
    (* Drop beginning of history *)
    let _, chunk_content = split at_origin in
    History.nexts chunk_content
  in
  try
    let filter = function
      | Chunk_parser.EOF -> false
      | _ -> true
    in
    let () = parser (Chunk_parser_utils.print_tokens (History.wrap_lexer ~filter ?bufpos history' lexer)) buf in
    let history = !history' in
    history, Outline_utils.Done, chunk_content history
  with
    | Outline_utils.Chunk (c,p) ->
        begin
          let history = !history' in
          let history = match History.backward history with
            | Some ((t,_,p'), history) when Lexing.(p.pos_cnum < p'.pos_cnum) -> 
                print_endline "refill"; history
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
    | exn -> 
        history, Outline_utils.Exception exn, []

module Chunked =
struct
  type item = Raw.sync * (int * Outline_utils.kind * Raw.item list * exn list)
  type sync = item History.sync
  type t = item History.t 

  let seek p t =
    let open Lexing in
    History.seek (fun (_,(_,_,l,_)) ->
      match l with
        | (_,start,_) :: _ when start.pos_cnum > p.pos_cnum -> -1
        | (_,_,curr) :: _ when curr.pos_cnum > p.pos_cnum -> 0
        | x :: xs ->
            let (_,_,curr) = List.fold_left (fun _ a -> a) x xs in
            if curr.pos_cnum > p.pos_cnum
            then 0
            else 1
        | [] -> 0
        | _ -> 1
    ) t
end

let parse_step ?(rollback=0) ?bufpos ?(exns=[]) history buf =
  Outline_utils.reset ~rollback ();
  let history', kind, tokens = parse_with history
    ~parser:Outline_parser.implementation
    ~lexer:Outline_lexer.token
    ?bufpos buf
  in
  let exns = match kind with
    | Outline_utils.Exception exn -> exn :: exns | _ -> exns
  in
  history', (History.sync_point history', (rollback, kind, tokens, exns))

let rec parse ?rollback ?bufpos (history,chunks) buf =
  let exns = match History.prev chunks with
    | Some (_,(_,_,_,exns)) -> exns
    | None -> []
  in
  match parse_step ?rollback ?bufpos ~exns history buf with
    | history', (_, (_, Outline_utils.Rollback, _, _)) ->
        let chunks', rollback =
          match History.backward chunks with
            | Some ((_, (rollback, _, _, _)), chunks') -> chunks', rollback
            | None -> chunks, 0
        in
        print_endline "SYNC PARSER";
        let history', chunks' = History.sync fst history' chunks' in
        let chunks', _ = History.split chunks' in
        parse ~rollback:(rollback + 1) ?bufpos (history',chunks') buf
    | history', (_, (_, Outline_utils.Unterminated, _, _)) ->
        history', chunks
    | history', item ->
        history', History.insert item chunks
