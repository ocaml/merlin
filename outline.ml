type token = Chunk_parser.token History.loc

let parse_with history ~parser ~lexer ~goteof ?bufpos buf =
  let origin = History.current_pos history in
  let history' = ref history in
  Outline_utils.get_offset :=
    (fun p ->
       let history = match History.backward !history' with
         | Some ((t,_,p'), history) when Lexing.(p.pos_cnum < p'.pos_cnum) -> 
             history
         | _ -> history
       in
       History.offset history);
  let chunk_content h =
    (* Drop end of history *)
    let end_of_chunk = History.cutoff h in
    let at_origin = History.seek_pos origin end_of_chunk in
    (* Drop beginning of history *)
    History.nexts at_origin
  in
  try
    let filter = function
      | Chunk_parser.EOF -> goteof := true; false
      | _ -> true
    in
    let lexer = History.wrap_lexer ~filter ?bufpos history' lexer in
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
          let defs = List.rev_map 
            (fun (_,defs) -> List.rev defs) 
            !Outline_utils.partial_definitions
          in
          let defs = List.flatten defs in
          Outline_utils.partial_definitions := [];
          let history = !history' in
          history, Outline_utils.Partial_definitions defs, chunk_content history
        end
    | exn ->
        history, Outline_utils.Exception exn, []

type item = int * Outline_utils.kind * token list * exn list
type sync = item History.sync
type t = item History.t 

let last_curr = List.fold_left (fun _ (_,_,curr) -> curr)

let rec last_position t =
  match History.prev t with
    | Some (_,_,(_,_,curr) :: xs, _) -> Some (last_curr curr xs)
    | None -> None
    | _ -> failwith "Outline.last_position: Invalid t"

let seek cmp t =
  let open Lexing in
  let t =
    History.seek begin fun (_,_,l,_) ->
      match l with
        | (_,start,_) :: _ when cmp start < 0 -> -1
        | (_,_,curr) :: xs when cmp curr < 0 || cmp (last_curr curr xs) < 0 -> 0
        | [] -> failwith "Outline.seek: Invalid t"
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

let parse_step ?(rollback=0) ?bufpos ?(exns=[]) ~goteof history buf =
  Outline_utils.reset ~rollback ();
  let history', kind, tokens = parse_with history
    ~parser:Outline_parser.implementation
    ~lexer:Outline_lexer.token
    ?bufpos ~goteof buf
  in
  Outline_utils.reset_get_offset ();
  let exns = match kind with
    | Outline_utils.Exception exn -> exn :: exns | _ -> exns
  in
  history',
  (match tokens with
    | [] -> None
    | _ -> Some (rollback, kind, tokens, exns))

let exns chunks =
  match History.prev chunks with
    | Some (_,_,_,exns) -> exns
    | None -> []

let rec parse ?rollback ?bufpos ~goteof tokens chunks buf =
  let exns = exns chunks in
  match parse_step ?rollback ?bufpos ~exns ~goteof tokens buf with
    | tokens', Some (_, Outline_utils.Rollback, _, _) ->
        let chunks', rollback =
          match History.backward chunks with
            | Some ((rollback, _, _, _), chunks') -> chunks', rollback
            | None -> chunks, 0
        in
        (*prerr_endline "SYNC PARSER";*)
        (*let tokens', chunks' = History.Sync.nearest fst tokens' chunks' in*)
        let chunks', _ = History.split chunks' in
        parse ~rollback:(rollback + 1) ?bufpos ~goteof tokens' chunks' buf
    | tokens', Some (_, Outline_utils.Unterminated, _, _) ->
        tokens', chunks
    | tokens', Some item ->
        tokens', History.insert item chunks
    | tokens', None ->
        tokens', chunks
