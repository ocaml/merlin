open Std

let make ~on_read ~input ~output =
  let rec read buf len =
    on_read input;
    try Unix.read input buf 0 len
    with Unix.Unix_error (Unix.EINTR,_,_) ->
      read buf len
  in
  let lexbuf  = Lexing.from_function read in
  let input   = Json.stream_from_lexbuf (Json.init_lexer ()) lexbuf in
  let input () = try Some (Stream.next input) with Stream.Failure -> None in
  let output  = Unix.out_channel_of_descr output in
  let output' = Json.to_channel output in
  let output json =
    output' json;
    output_char output '\n';
    flush output
  in
  input, output

