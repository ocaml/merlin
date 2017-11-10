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


let with_include_dir path f =
  let saved = !Clflags.include_dirs in
  let restore () = Clflags.include_dirs := saved in
  Clflags.include_dirs := path;
  let result =
    begin
      try
        f ()
      with
      | e ->
         restore ();
         raise e
    end
  in
  restore ();
  result


let rewrite _trace cfg parsetree =
  let ppx = cfg.ocaml.ppx @ Ppxsetup.command_line cfg.merlin.packages_ppx in
  let prev_dir = Sys.getcwd () in
  let restore () =
    if not (change_directory prev_dir) then
      ignore (change_directory "/")
  in
  ignore (change_directory cfg.query.directory);
  (* add include path attribute to the parsetree *)
  with_include_dir (Mconfig.build_path cfg) @@ fun () ->
  match Pparse.apply_rewriters ~ppx ~tool_name:"merlin" parsetree with
  | parsetree ->
    restore ();
    cfg, parsetree
  | exception exn ->
    Logger.logf "Mppx" "rewrite" "failed with %t" (fun () ->
        match Location.error_of_exn exn with
        | None | Some `Already_displayed -> Printexc.to_string exn
        | Some (`Ok err) -> err.Location.msg
      );
    Msupport.raise_error exn;
    restore ();
    cfg, parsetree
