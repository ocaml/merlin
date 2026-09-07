let cursor = "█"

let () =
  match Merlin_commands.New_commands.parse_position Sys.argv.(2) with
  | `Start ->
    let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
    print_endline (cursor ^ source)
  | `End ->
    let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
    print_endline (source ^ cursor)
  | `Offset i ->
    let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
    let pre = String.sub source 0 i in
    let post = String.sub source i (String.length source - i) in
    print_endline (pre ^ cursor ^ post)
  | `Logical (line, char) ->
    let lines = In_channel.with_open_bin Sys.argv.(1) In_channel.input_lines in
    List.iteri
      (fun i l ->
        if i + 1 = line then
          let pre = String.sub l 0 char in
          let post =
            String.sub l
              (Int.min (String.length l - 1) (char + 1))
              (Int.max 0 (String.length l - char - 1))
          in
          print_endline (pre ^ cursor ^ post)
        else print_endline l)
      lines
