open Mconfig

let change_directory dir =
  Logger.log "Mppx" "changing_directory" dir;
  match Sys.chdir dir with
  | () -> true
  | exception exn ->
    Logger.logf "Mppx" "changing directory"
      "chang_directory %S failed with %t" dir
      (fun () -> Printexc.to_string exn);
    false


let rewrite _trace cfg parsetree =
  let ppx = cfg.ocaml.ppx @ Ppxsetup.command_line cfg.merlin.packages_ppx in
  let prev_dir = Sys.getcwd () in
  let restore () =
    if not (change_directory prev_dir) then
      ignore (change_directory "/")
  in
  ignore (change_directory cfg.query.directory);
  match Pparse.apply_rewriters ~ppx ~tool_name:"merlin" parsetree with
  | parsetree ->
    restore ();
    cfg, parsetree
  | exception exn ->
    Logger.logf "Mppx" "rewrite" "failed with %t" (fun () ->
        match Location.error_of_exn exn with
        | None -> Printexc.to_string exn
        | Some err -> err.Location.msg
      );
    Msupport.raise_error exn;
    restore ();
    cfg, parsetree
