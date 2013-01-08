
let parse_with history parser lexer buf =
  let origin = History.current_pos history in
  let history' = ref history in
  let chunk_content h =
    let open History in
    (* Drop end of history *)
    let end_of_chunk, _ = split h in
    let at_origin = seek (this_position origin) end_of_chunk in
    (* Drop beginning of history *)
    let _, chunk_content = split at_origin in
    chunk_content
  in
  try
    let () = parser (History.wrap history' lexer) buf in
    Parsing.clear_parser ();
    let history = !history' in
    history, Outline_utils.Done, chunk_content history
  with
    | Outline_utils.Chunk (c,p) ->
        begin
          Parsing.clear_parser ();
          let history =
            if p <> buf.Lexing.lex_curr_p
            then (prerr_endline "refill";
                  snd (History.backward !history'))
            else !history'
          in
          history, c, chunk_content history
        end
    | Sys.Break ->
        begin
          Parsing.clear_parser ();
          let history = !history' in
          History.(seek (this_position origin) history),
          Outline_utils.Unterminated,
          History.empty
        end
    | exn ->
        Parsing.clear_parser ();
        raise exn

(*let parse_with ?token parser lexer buf =
  let tokens = ref [] in
  let fake_token = ref token in
  let fake_position = ref None in
  let set_position buf start curr =
    Lexing.(buf.lex_start_p <- start; buf.lex_curr_p <- curr)
  in
  let wrap buf =
    let ret =
      match !fake_token with
        | Some (token,start,curr) ->
            fake_position := Some (Lexing.(buf.lex_start_p, buf.lex_curr_p));
            fake_token := None;
            set_position buf start curr;
            token
        | None ->
      match !fake_position with
        | Some (start,curr) ->
            fake_position := None;
            set_position buf start curr;
            lexer buf
        | None ->
            lexer buf
    in
    tokens := ret :: !tokens;
    ret
  in
  try
    let () = parser wrap buf in
    Parsing.clear_parser ();
    None, !tokens, Outline_utils.Done
  with
    | Outline_utils.Chunk (c,p) ->
        begin
          Parsing.clear_parser ();
          match !tokens with
          | t :: ts when p <> buf.Lexing.lex_curr_p ->
              prerr_endline "refill";
              Some t, ts, c
          | ts ->
              None, ts, c
        end
    | exn ->
        Parsing.clear_parser ();
        raise exn*)
(* val refold : ('a -> 'a option) -> 'a -> 'a *)
let rec refold f a =
  match f a with
    | Some a' -> refold f a'
    | None    -> a

let _ =
  let buf = Lexing.from_channel stdin in
  let parse history =
    parse_with history Outline_parser.implementation Outline_lexer.token buf
  in
  let rec loop history =
    let history', chunk, content = parse history in
    let before, after = History.split history' in
    ignore (refold
              (fun h -> match History.backward h with
                | Some (t,_,_), h' ->
                    Printf.printf "%s " (Outline_token.to_string t);
                    
                    Some h'
                | None, h' ->
                    None
              ) history');
    Printf.printf "\n%!";
    loop after
  in
  loop History.empty
  (*List.iter (fun tok -> print_string (Outline_token.to_string tok); print_char ' ') tokens*)
