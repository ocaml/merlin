type read_result = Success | Eof

let read_line_opt () = try Some (read_line ()) with End_of_file -> None
let delimiter1 = Sys.argv.(1)
let delimiter2 = Sys.argv.(2)
let diff_file = Sys.argv.(3)

let rec loop () =
  let rec stdin_to_file ~until file_oc =
    match read_line_opt () with
    | None -> Eof
    | Some line ->
        if String.equal line until then Success
        else (
          output_string file_oc (line ^ "\n");
          stdin_to_file ~until file_oc)
  in
  let label1 = read_line_opt () in
  let tmp1 = Filename.temp_file "tmp1" "" in
  let oc = open_out tmp1 in
  let read_result1 = stdin_to_file ~until:delimiter1 oc in
  close_out oc;
  let label2 = read_line_opt () in
  let tmp2 = Filename.temp_file "tmp2" "" in
  let oc = open_out tmp2 in
  let read_result2 = stdin_to_file ~until:delimiter2 oc in
  close_out oc;
  match (label1, read_result1, label2, read_result2) with
  | Some label1, Success, Some label2, Success ->
      let diff_cmd =
        Printf.sprintf
          "diff -U 5 --label=\"%s\" --label=\"%s\" \"%s\" \"%s\" >> %s" label1
          label2 tmp1 tmp2 diff_file
      in
      let _ = Sys.command diff_cmd in
      loop ()
  | Some _, Success, Some _, Eof ->
      raise (Failure "EOF before reaching delimiter2 when reading second JSON.")
  | Some _, Success, None, _ ->
      raise (Failure "EOF before reaching delimiter2 when second label.")
  | Some _, Eof, _, _ ->
      raise (Failure "EOF before reaching delimiter1 when reading first JSON.")
  | None, _, _, _ -> ()

let () = loop ()
