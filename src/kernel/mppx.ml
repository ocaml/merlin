open Mconfig

let {Logger. log} = Logger.for_section "Mppx"

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


let rewrite cfg parsetree =
  let ppx = cfg.ocaml.ppx in
  (* add include path attribute to the parsetree *)
  with_include_dir (Mconfig.build_path cfg) @@ fun () ->
  match
    Pparse.apply_rewriters ~restore:false ~ppx ~tool_name:"merlin" parsetree
  with
  | parsetree -> parsetree
  | exception exn ->
    log ~title:"rewrite" "failed with %a" Logger.fmt (fun fmt ->
      match Location.error_of_exn exn with
      | None | Some `Already_displayed ->
        Format.fprintf fmt "%s" (Printexc.to_string exn)
      | Some (`Ok err) ->
        Location.print_main fmt err
    );
    Msupport.raise_error exn;
    parsetree
