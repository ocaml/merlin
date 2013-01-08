let _ =
  let buf = Lexing.from_channel stdin in
  let rec loop chunks history =
    let history', chunk, data = Outline.parse_step history buf in
    let before, after = History.split history' in
    let chunks = Chunk.append_step chunk data chunks in
    ignore (Misc.refold (fun h -> match History.backward h with
      | Some ((t,_,_), h') ->
          Printf.printf "%s " (Outline.token_to_string t);
          Some h'
      | None -> None
    ) history);
    Printf.printf "\n%!";
    loop chunks after
  in
  loop Chunk.empty History.empty
