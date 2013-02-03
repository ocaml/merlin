type io = Json.json Stream.t * (Json.json -> unit)

let make ~input ~output =
  let input  = Json.stream_from_channel input in
  let output' = Json.to_channel output in
  let output json =
    output' json;
    print_newline ()
  in
  input, output

let log ~dest (input,output) =
  let log_input json = Printf.fprintf dest "> %s\n%!" (Json.to_string json); json in
  let log_output json = Printf.fprintf dest "< %s\n%!" (Json.to_string json); json in
  let input' =
    Stream.from
    begin fun _ ->
      try Some (log_input (Stream.next input))
      with Stream.Failure -> None
    end
  in
  let output' json = output (log_output json) in
  input', output'

let return l = `List (`String "return" :: l)

let error_catcher = ref (fun _ -> None)
let fail = function
  | Failure s -> `List [`String "failure"; `String s]
  | exn -> match !error_catcher exn with
      | Some error -> `List [`String "error"; error]
      | None -> `List [`String "exception"; `String (Printexc.to_string exn)]

let pos_to_json pos =
  Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                  "col", `Int (pos.pos_cnum - pos.pos_bol)])
                  (*"offset", `Int pos.pos_cnum])*)

let pos_of_json = function
  | `Assoc props ->
    begin try match List.assoc "line" props, List.assoc "col" props with
      | `Int line, `Int col -> (line, col)
      | _ -> failwith "Incorrect position"
    with Not_found -> failwith "Incorrect position"
    end
  | _ -> failwith "Incorrect position"

