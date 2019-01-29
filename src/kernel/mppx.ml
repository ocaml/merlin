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

open Mconfig

let {Logger. log} = Logger.for_section "Mppx"

let change_directory dir =
  log ~title:"changing_directory" "%s" dir;
  match Sys.chdir dir with
  | () -> true
  | exception exn ->
    log ~title:"changing directory"
      "change_directory %S failed with %a" dir Logger.exn exn;
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


let rewrite cfg parsetree =
  let ppx = cfg.ocaml.ppx @
            List.map
              (fun ppx -> {Std. workdir = cfg.query.directory; workval = ppx })
              (Ppxsetup.command_line cfg.merlin.packages_ppx)
  in
  let prev_dir = Sys.getcwd () in
  let restore () =
    if not (change_directory prev_dir) then
      ignore (change_directory "/")
  in
  (* add include path attribute to the parsetree *)
  with_include_dir (Mconfig.build_path cfg) @@ fun () ->
  match Pparse.apply_rewriters ~ppx ~tool_name:"merlin" parsetree with
  | parsetree ->
    restore ();
    cfg, parsetree
  | exception exn ->
    log ~title:"rewrite" "failed with %t"
      (fun () -> match Location.error_of_exn exn with
         | None | Some `Already_displayed -> Printexc.to_string exn
         | Some (`Ok err) -> err.Location.msg
      );
    Msupport.raise_error exn;
    restore ();
    cfg, parsetree
