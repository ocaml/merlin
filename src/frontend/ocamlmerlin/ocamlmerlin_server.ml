let merlin_timeout =
  try float_of_string (Sys.getenv "MERLIN_TIMEOUT") with _ -> 600.0

module Server = struct
  let process_request { Os_ipc.wd; environ; argv; context = _ } shared =
    match Array.to_list argv with
    | "stop-server" :: _ -> raise Exit
    | args -> New_merlin.run ~new_env:(Some environ) (Some wd) args shared

  let process_client client shared =
    let context = client.Os_ipc.context in
    Os_ipc.context_setup context;
    let close_with return_code =
      flush_all ();
      Os_ipc.context_close context ~return_code
    in
    match process_request client shared with
    | code -> close_with code
    | exception Exit ->
      close_with (-1);
      raise Exit
    | exception exn ->
      Logger.log ~section:"server" ~title:"process failed" "%a" Logger.exn exn;
      close_with (-1)

  let server_accept merlinid server =
    let rec loop total =
      let merlinid' = File_id.get Sys.executable_name in
      if total > merlin_timeout || not (File_id.check merlinid merlinid') then
        None
      else
        let timeout = max 10.0 (min 60.0 (merlin_timeout -. total)) in
        match Os_ipc.server_accept server ~timeout with
        | Some _ as result -> result
        | None -> loop (total +. timeout)
    in
    match Os_ipc.server_accept server ~timeout:1.0 with
    | Some _ as result -> result
    | None -> loop 1.0

  let rec loop merlinid server shared =
    match server_accept merlinid server with
    | None ->
      (* Timeout *)
      ()
    | Some client ->
      let continue =
        match process_client client shared with
        | exception Exit -> false
        | () -> true
      in
      if continue then loop merlinid server shared

  let start socket_path socket_fd =
    match Os_ipc.server_setup socket_path socket_fd with
    | None -> Logger.log ~section:"server" ~title:"cannot setup listener" ""
    | Some server ->
      (* If the client closes its connection, don't let it kill us with a SIGPIPE. *)
      if Sys.unix then Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

      let shared = Mpipeline.create_shared () in
      let domain_typer = Domain.spawn @@ Mpipeline.domain_typer shared in
      loop (File_id.get Sys.executable_name) server shared;
      Mpipeline.close_typer shared;
      Domain.join domain_typer;
      Os_ipc.server_close server
end

let main () =
  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args ->
    let shared = Mpipeline.create_shared () in
    let domain_typer = Domain.spawn @@ Mpipeline.domain_typer shared in
    let vexit = New_merlin.run ~new_env:None None args shared in
    Mpipeline.close_typer shared;
    Domain.join domain_typer;
    exit vexit
  | "old-protocol" :: args -> Old_merlin.run args
  | [ "server"; socket_path; socket_fd ] -> Server.start socket_path socket_fd
  | ("-help" | "--help" | "-h" | "server") :: _ ->
    Printf.eprintf
      "Usage: %s <frontend> <arguments...>\n\
       Select the merlin frontend to execute. Valid values are:\n\n\
       - 'old-protocol' executes the merlin frontend from previous version.\n\
      \  It is a top level reading and writing commands in a JSON form.\n\n\
       - 'single' is a simpler frontend that reads input from stdin,\n\
      \  processes a single query and outputs result on stdout.\n\n\
       - 'server' works like 'single', but uses a background process to\n\
      \  speedup processing.\n\
       If no frontend is specified, it defaults to 'old-protocol' for\n\
       compatibility with existing editors.\n"
      Sys.argv.(0)
  | args -> Old_merlin.run args

let () =
  Lib_config.Json.set_pretty_to_string Yojson.Basic.pretty_to_string;
  let `Log_file_path log_file, `Log_sections sections = Log_info.get () in
  Logger.with_log_file log_file ~sections main
