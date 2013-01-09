let _ =
  let buf = Lexing.from_channel stdin in
  let rec loop tokens chunks parsed =
    let tokens,chunks = Outline.parse (tokens,chunks) buf in
    let parsed = Chunk.append chunks parsed in
    (*let chunks = Chunk.append_step chunk data chunks in
    ignore (Misc.refold (fun h -> match History.backward h with
      | Some ((t,_,_), h') ->
          Printf.printf "%s " (Outline.token_to_string t);
          Some h'
      | None -> None
    ) history);*)
    Printf.printf "\n%!";
    loop tokens chunks parsed
  in
  loop History.empty History.empty History.empty
