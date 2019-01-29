(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

let merlin_timeout =
  try float_of_string (Sys.getenv "MERLIN_TIMEOUT")
  with _ -> 600.0

module Server = struct

  let rec protect_eintr f =
    match f () with
    | exception (Unix.Unix_error(Unix.EINTR, _, _)) -> protect_eintr f
    | result -> result

  let process_request {Os_ipc. wd; environ; argv; context = _}  =
    match Array.to_list argv with
    | "stop-server" :: _ -> raise Exit
    | args -> New_merlin.run environ (Some wd) args

  let process_client client =
    let context = client.Os_ipc.context in
    Os_ipc.context_setup context;
    let close_with return_code =
      flush_all ();
      Os_ipc.context_close context ~return_code
    in
    match process_request client with
    | code -> close_with code
    | exception Exit ->
      close_with (-1);
      raise Exit
    | exception exn ->
      Logger.log ~section:"server" ~title:"process failed" "%a"
        Logger.exn exn;
      close_with (-1)

  let server_accept merlinid server =
    let rec loop total =
      Mocaml.flush_caches ~older_than:300.0 ();
      let merlinid' = File_id.get Sys.executable_name in
      if total > merlin_timeout ||
         not (File_id.check merlinid merlinid') then
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
      Logger.log ~section:"server" ~title:"cannot setup listener" ""
    | Some server ->
      loop (File_id.get Sys.executable_name) server;
      Os_ipc.server_close server
end

let main () =
  (* Setup env for extensions *)
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args -> exit (New_merlin.run ("PATH=" ^ Unix.getenv "PATH" ^ "\000") None args)
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
