open Mconfig

let rewrite _trace config parsetree =
  let ppx = config.ocaml.ppx in
  match Pparse.apply_rewriters ~ppx ~tool_name:"merlin" parsetree with
  | parsetree -> config, parsetree
  | exception exn ->
    Merlin_support.raise_error exn;
    config, parsetree
