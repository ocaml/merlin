open Mconfig

let { Logger.log } = Logger.for_section "Mppx"

let with_include_dir ~visible_path ~hidden_path f =
  let saved_visible = !Clflags.include_dirs in
  let saved_hidden = !Clflags.hidden_include_dirs in
  let restore () =
    Clflags.include_dirs := saved_visible;
    Clflags.hidden_include_dirs := saved_hidden
  in
  Clflags.include_dirs := visible_path;
  Clflags.hidden_include_dirs := hidden_path;
  let result =
    begin
      try f ()
      with e ->
        restore ();
        raise e
    end
  in
  restore ();
  result

let rewrite parsetree cfg =
  let ppx = cfg.ocaml.ppx in
  (* add include path attribute to the parsetree *)
  with_include_dir ~visible_path:(Mconfig.build_path cfg)
    ~hidden_path:(Mconfig.hidden_build_path cfg)
  @@ fun () ->
  match
    Pparse.apply_rewriters ~restore:false ~ppx ~tool_name:"merlin" parsetree
  with
  | parsetree -> parsetree
  | exception exn ->
    log ~title:"rewrite" "failed with %a" Logger.fmt (fun fmt ->
        match Location.error_of_exn exn with
        | None | Some `Already_displayed ->
          Format.fprintf fmt "%s" (Printexc.to_string exn)
        | Some (`Ok err) -> Location.print_main fmt err);
    Msupport.raise_error exn;
    parsetree
