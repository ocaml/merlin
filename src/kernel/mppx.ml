open Mconfig

let rewrite _trace cfg parsetree =
  let ppx = cfg.ocaml.ppx @ Ppxsetup.command_line cfg.merlin.packages_ppx in
  match Pparse.apply_rewriters ~ppx ~tool_name:"merlin" parsetree with
  | parsetree -> cfg, parsetree
  | exception exn ->
    Msupport.raise_error exn;
    cfg, parsetree
