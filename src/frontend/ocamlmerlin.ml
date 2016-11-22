let main () =
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args -> New_merlin.run args
  | "daemon" :: _ -> Daemon_merlin.start_daemon ()
  | "stop-daemon" :: _ -> failwith "TODO"
  | "old-protocol" :: args -> Old_merlin.run args
  | ("-help" | "--help" | "-h") :: _ ->
    Printf.eprintf
      "Usage: %s <frontend> <arguments...>\n\
       Select the merlin frontend to execute. Valid values are:\n\
      \n- 'old-protocol' executes the merlin frontend from previous version.\n\
      \  It is a top level reading and writing commands in a JSON form.\n\
      \n- 'single' is a simpler frontend that reads input from stdin,\n\
      \  processes a single query and outputs result on stdout.\n\
      \n- 'daemon' works like 'single', but uses a background process to\n\
      \  speedup processing.\n\
      \n- 'stop-daemon' takes no argument and stops the background\n\
      \  process started by a previous call to 'daemon'\n\n\
       If no frontend is specified, it defaults to 'old-protocol' for\n\
       compatibility with existing editors.\n"
      Sys.argv.(0)
  | args -> Old_merlin.run args

let () =
  let log_file =
    match Sys.getenv "MERLIN_LOG" with
    | exception Not_found -> None
    | file -> Some file
  in
  Logger.with_log_file log_file main
