module Server = struct

  let rec protect_eintr f =
    match f () with
    | exception (Unix.Unix_error(Unix.EINTR, _, _)) -> protect_eintr f
    | result -> result

  let process_request = function
    | [|"stop-server"|] ->
      raise Exit
    | argv ->
      let args = Array.to_list argv in
      New_merlin.run args

  (* Work with triplet (stdin,stdout,stderr) *)
  let io_close (a,b,c) = Unix.(close a; close b; close c)

  let io_dup (a,b,c) = Unix.(dup a, dup b, dup c)

  let io_dup2 (a0,b0,c0) (a1,b1,c1) = Unix.(dup2 a0 a1; dup2 b0 b1; dup2 c0 c1)

  let io_move src dst = io_dup2 src dst; io_close src

  let process_client fd =
    match Os_ipc.connect fd with
    | None -> ()
    | Some client ->
      let io_std = Unix.(stdin, stdout, stderr) in
      let io_std' = io_dup io_std in
      io_move Os_ipc.(client.stdin, client.stdout, client.stderr) io_std;
      let result =
        match process_request client.Os_ipc.argv with
        | code -> String.make 1 (Char.chr code)
        | exception Exit -> raise Exit
        | exception _exn -> "\x01"
      in
      io_move io_std' io_std;
      protect_eintr (fun () -> ignore (Unix.write fd result 0 1))

  let rec loop merlinid fd =
    match protect_eintr (fun () -> Unix.select [fd] [] [] 180.0) with
    | [], [], [] -> (* Timeout *)
      ()
    | _ ->
      match Unix.accept fd with
      | exception exn ->
        Unix.close fd
      | clientfd, _ ->
        let continue =
          match process_client clientfd with
          | exception Exit -> false
          | exception _exn -> true
          | () -> true
        in
        Unix.close clientfd;
        if continue then
          let merlinid' = Misc.file_id Sys.executable_name in
          if Misc.file_id_check merlinid merlinid' then
            loop merlinid fd

  let start socket_path socket_fd =
    let fd = Os_ipc.get_fd socket_fd in
    let merlinid = Misc.file_id Sys.executable_name in
    loop merlinid fd;
    Unix.unlink socket_path;
    Unix.close fd
end

let main () =
  match List.tl (Array.to_list Sys.argv) with
  | "single" :: args -> exit (New_merlin.run args)
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
