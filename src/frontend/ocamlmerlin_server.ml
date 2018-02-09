let merlin_timeout =
  try float_of_string (Sys.getenv "MERLIN_TIMEOUT")
  with _ -> 600.0

module Server = struct

  let rec protect_eintr f =
    match f () with
    | exception (Unix.Unix_error(Unix.EINTR, _, _)) -> protect_eintr f
    | result -> result

  let process_arguments argv =
    let rec aux acc = function
      | "" :: args | ([] as args) -> (acc, args)
      | var :: rest -> aux (var :: acc) rest
    in
    let wd, args =
      match Array.to_list argv with
      | [] -> "", []
      | wd :: rest -> wd, rest
    in
    let env, args = aux [] args in
    (wd, env, args)


  let process_request argv =
    match process_arguments argv with
    | _, _, ("stop-server" :: _) -> raise Exit
    | wd, env, args ->
      begin try Sys.chdir wd
        with _ ->
          Logger.logf "Ocamlmerlin_server" "Server.process_request"
            "cannot change working directory to %S" wd
      end;
      New_merlin.run env args

  let process_client client =
    let context = client.Os_ipc.context in
    Os_ipc.context_setup context;
    let close_with return_code =
      flush_all ();
      Os_ipc.context_close context ~return_code
    in
    match process_request client.Os_ipc.argv with
    | code -> close_with code
    | exception Exit ->
      close_with (-1);
      raise Exit
    | exception exn ->
      Logger.log "server" "process failed"
        (Printexc.to_string exn);
      close_with (-1)

  let server_accept merlinid server =
    let rec loop total =
      Mocaml.flush_caches ~older_than:300.0 ();
      let merlinid' = Misc.file_id Sys.executable_name in
      if total > merlin_timeout ||
         not (Misc.file_id_check merlinid merlinid') then
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

  let rec loop merlinid server =
    match server_accept merlinid server with
    | None -> (* Timeout *)
      ()
    | Some client ->
      let continue =
        match process_client client with
        | exception Exit -> false
        | () -> true
      in
      if continue then loop merlinid server

  let start socket_path socket_fd =
    match Os_ipc.server_setup socket_path socket_fd with
    | None ->
      Logger.log "server" "cannot setup listener" ""
    | Some server ->
      loop (Misc.file_id Sys.executable_name) server;
      Os_ipc.server_close server
end

let main () =
  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args -> exit (New_merlin.run [] args)
  | "old-protocol" :: args -> Old_merlin.run args
  | ["server"; socket_path; socket_fd] -> Server.start socket_path socket_fd
  | ("-help" | "--help" | "-h" | "server") :: _ ->
    Printf.eprintf
      "Usage: %s <frontend> <arguments...>\n\
       Select the merlin frontend to execute. Valid values are:\n\
      \n- 'old-protocol' executes the merlin frontend from previous version.\n\
      \  It is a top level reading and writing commands in a JSON form.\n\
      \n- 'single' is a simpler frontend that reads input from stdin,\n\
      \  processes a single query and outputs result on stdout.\n\
      \n- 'server' works like 'single', but uses a background process to\n\
      \  speedup processing.\n\
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
