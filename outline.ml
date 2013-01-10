module Utils = Outline_utils

module Raw =
struct
  type token = Chunk_parser.token
  type item = token History.loc  
  type sync = item History.sync
  type t = item History.t 
end

let parse_with history ~parser ~lexer buf =
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
    let () = parser (Chunk_parser_utils.print_tokens (History.wrap_lexer ~filter history' lexer)) buf in
    let history = !history' in
    history, Outline_utils.Done, chunk_content history
  with
    | Outline_utils.Chunk (c,p) ->
        begin
          let history = !history' in
          let history = match History.backward history with
            | Some ((t,_,p'), history) when p <> p' -> 
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

module Chunked =
struct
  type item = Raw.sync * (int * Outline_utils.kind * Raw.item list)
  type sync = item History.sync
  type t = item History.t 
end

let parse_step ?(rollback=0) history buf =
  Outline_utils.reset ~rollback ();
  let history', kind, tokens = parse_with history
    ~parser:Outline_parser.implementation
    ~lexer:Outline_lexer.token
    buf
  in
  history', (History.sync_point history', (rollback, kind, tokens))

let rec parse ?rollback (history,chunks) buf =
  match parse_step ?rollback history buf with
    | history', (_, (_, Outline_utils.Rollback, _)) ->
        let chunks', rollback =
          match History.backward chunks with
            | Some ((_, (rollback, _, _)), chunks') -> chunks', rollback
            | None -> chunks, 0
        in
        let history', chunks' = History.sync fst history' chunks' in
        let chunks', _ = History.split chunks' in
        parse ~rollback:(rollback + 1) (history',chunks') buf
    | history', (_, (_, Outline_utils.Unterminated, _)) ->
        history', chunks
    | history', item ->
        history', History.insert item chunks
