let get () =
  let log_file, sections =
    match String.split_on_char ',' (Sys.getenv "MERLIN_LOG") with
    | value :: sections -> (Some value, sections)
    | [] -> (None, [])
    | exception Not_found -> (None, [])
  in
  (`Log_file_path log_file, `Log_sections sections)
