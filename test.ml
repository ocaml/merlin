(* val refold : ('a -> 'a option) -> 'a -> 'a *)
let rec refold f a =
  match f a with
    | Some a' -> refold f a'
    | None    -> a

let _ =
  let buf = Lexing.from_channel stdin in
  let rec loop chunks history =
    let history', chunk, data = Outline.parse history buf in
    let before, after = History.split history' in
    let chunks = Chunk.append chunk data chunks in
    ignore (refold
              (fun h -> match History.backward h with
                | Some (t,_,_), h' ->
                    Printf.printf "%s " (Outline.token_to_string t);
                    
                    Some h'
                | None, h' ->
                    None
              ) history');
    Printf.printf "\n%!";
    loop chunks after
  in
  loop Chunk.empty History.empty
  (*List.iter (fun tok -> print_string (Outline_token.to_string tok); print_char ' ') tokens*)
