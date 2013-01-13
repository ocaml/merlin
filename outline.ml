module Raw =
struct
  type token = Chunk_parser.token
  type item = token History.loc  
  type sync = item History.sync
  type t = item History.t 
end

let parse_with history ~parser ~lexer ~goteof ?bufpos buf =
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
      | Chunk_parser.EOF -> goteof := true; false
      | _ -> true
    in
    let lexer = History.wrap_lexer ~filter ?bufpos history' lexer in
    (*let lexer = Chunk_parser_utils.print_tokens ~who:"outline" lexer in*)
    let () = parser lexer buf in
    let history = !history' in
    history, Outline_utils.Done, chunk_content history
  with
    | Outline_utils.Chunk (c,p) ->
        begin
          let history = !history' in
          let history = match History.backward history with
            | Some ((t,_,p'), history) when Lexing.(p.pos_cnum < p'.pos_cnum) -> 
                prerr_endline "refill"; history
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

  let last_curr = List.fold_left (fun _ (_,_,curr) -> curr)

  let rec last_position t =
    match History.prev t with
      | Some (_,(_,_,(_,_,curr) :: xs, _)) -> Some (last_curr curr xs)
      | None -> None
      | _ -> failwith "Outline.Chunked.last_position: Invalid Chunked.t"

  let seek cmp t =
    let open Lexing in
    let t =
      History.seek begin fun (_,(_,_,l,_)) ->
        match l with
          | (_,start,_) :: _ when cmp start < 0 -> -1
          | (_,_,curr) :: xs when cmp curr < 0 || cmp (last_curr curr xs) < 0 -> 0
          | [] -> failwith "Outline.Chunked.seek: Invalid Chunked.t"
          | _ -> 1
      end t
    in
    match History.backward t with
      | Some (_,t) -> t
      | None -> t

  let seek_line (line,col) =
    Lexing.(seek (fun pos ->
      match compare line pos.pos_lnum with
        | 0 -> compare col (pos.pos_cnum - pos.pos_bol)
        | n -> n))

  let seek_offset offset =
    seek (fun pos -> compare offset pos.Lexing.pos_cnum)
end

let parse_step ?(rollback=0) ?bufpos ?(exns=[]) ~goteof history buf =
  Outline_utils.reset ~rollback ();
  let history', kind, tokens = parse_with history
    ~parser:Outline_parser.implementation
    ~lexer:Outline_lexer.token
    ?bufpos ~goteof buf
  in
  let exns = match kind with
    | Outline_utils.Exception exn -> exn :: exns | _ -> exns
  in
  history',
  (match tokens with
    | [] -> None
    | _ -> Some (History.sync_point history', (rollback, kind, tokens, exns)))

let rec parse ?rollback ?bufpos ~goteof (history,chunks) buf =
  let exns = match History.prev chunks with
    | Some (_,(_,_,_,exns)) -> exns
    | None -> []
  in
  match parse_step ?rollback ?bufpos ~exns ~goteof history buf with
    | history', Some (_, (_, Outline_utils.Rollback, _, _)) ->
        let chunks', rollback =
          match History.backward chunks with
            | Some ((_, (rollback, _, _, _)), chunks') -> chunks', rollback
            | None -> chunks, 0
        in
        prerr_endline "SYNC PARSER";
        let history', chunks' = History.sync fst history' chunks' in
        let chunks', _ = History.split chunks' in
        parse ~rollback:(rollback + 1) ?bufpos ~goteof (history',chunks') buf
    | history', Some (_, (_, Outline_utils.Unterminated, _, _)) ->
        history', chunks
    | history', Some item ->
        history', History.insert item chunks
    | history', None ->
        history', chunks
