let process_request = function
  | [|"stop-daemon"|] ->
    raise Exit
  | argv ->
    let args = Array.to_list argv in
    New_merlin.run args

let process_client fd =
  match Os_ipc.connect fd with
  | None -> ()
  | Some {Os_ipc. stdin; stdout; stderr; argv} ->
    let stdin'  = Unix.dup Unix.stdin  in
    let stdout' = Unix.dup Unix.stdout in
    let stderr' = Unix.dup Unix.stderr in
    Unix.dup2 stdin Unix.stdin;
    Unix.dup2 stdout Unix.stdout;
    Unix.dup2 stderr Unix.stderr;
    Unix.close stdin;
    Unix.close stdout;
    Unix.close stderr;
    let cleanup () =
      Unix.dup2 stdin'  Unix.stdin;
      Unix.dup2 stdout' Unix.stdout;
      Unix.dup2 stderr' Unix.stderr;
      Unix.close stdin';
      Unix.close stdout';
      Unix.close stderr';
    in
    let result =
      match process_request argv with
      | () -> "\x00"
      | exception _exn -> "\x01"
    in
    cleanup ();
    let rec write_result () =
      match Unix.write fd result 0 1 with
      | exception (Unix.Unix_error(Unix.EINTR, _, _)) ->
        write_result ()
      | _ -> ()
    in
    write_result ()

let rec daemon_loop fd =
  match Unix.accept fd with
  | exception exn ->
    Unix.close fd
  | clientfd, _ ->
    let exit =
      match process_client clientfd with
      | () -> false
      | exception Exit -> true
      | exception _exn -> false
    in
    Unix.close clientfd;
    if not exit then
      daemon_loop fd

let start_daemon fdnum =
  let fd = Os_ipc.get_fd fdnum in
  daemon_loop fd
