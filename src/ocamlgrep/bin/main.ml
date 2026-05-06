(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2000-2026 LexiFi *)
(*
   Command-line interface and entry point for the standalone 'ocamlgrep'
   command. This is a thin wrapper around the [Ocamlgrep] library, mirroring
   the original ocamlgrep CLI so users can run it independently of the
   merlin server.
*)

open Printf

type color =
  | Yellow
  | Red
  | Green

(* Colors are emitted unless the user opts out via the standard NO_COLOR env
   variable (https://no-color.org/). This keeps the snapshot tests readable
   without changing the default interactive behavior. *)
let use_colors =
  match Sys.getenv_opt "NO_COLOR" with
  | Some s when s <> "" -> false
  | _ -> true

let color c fmt =
  if use_colors then
    sprintf
      ("\027[1;%dm" ^^ fmt ^^ "\027[0m")
      (match c with Yellow -> 33 | Red -> 31 | Green -> 32)
  else
    sprintf fmt

let warn msg =
  eprintf "%s: %s\n%!" (color Yellow "Warning") msg

let print_finding_with_color_range (finding : Scan.finding) =
  let file_color = color Green "%s" finding.source in
  let i_color = color Yellow "%d" finding.i in
  let s_color =
    let len = String.length finding.s in
    if finding.c2 > len || finding.c1 > len then
      sprintf
        " Skipping this line with wrong indexes -- Maybe you should think \
         about recompiling this file."
    else
      String.sub finding.s 0 finding.c1
      ^ color Red "%s" (String.sub finding.s finding.c1 (finding.c2 - finding.c1))
      ^ String.sub finding.s finding.c2 (String.length finding.s - finding.c2)
  in
  printf "%s:%s:%s\n%!" file_color i_color s_color

let handle_event (ev : Scan.event) =
  match ev with
  | Scan_file _path -> ()
  | Warning msg -> warn msg
  | Finding finding -> print_finding_with_color_range finding

let main () =
  let query = ref None in
  let usage_msg = "Usage: ocamlgrep <pattern>" in
  Arg.parse [] (fun s -> query := Some s) usage_msg;
  let paths =
    match Paths.identify_dune_project () with
    | Error msg -> failwith msg
    | Ok paths -> paths
  in
  Paths.init paths;
  match !query with
  | None ->
    Arg.usage [] usage_msg;
    exit 0
  | Some s -> Scan.incremental_search paths handle_event s

let () =
  try
    (* Merlin's vendored [Load_path.init] asserts the presence of a
       [Local_store] scope. The merlin server provides one through its
       pipeline; for the standalone binary we set one up explicitly. *)
    Local_store.with_store (Local_store.fresh ()) main
  with exn ->
    let s =
      match exn with
      | Failure s | Sys_error s -> s
      | exn -> Printexc.to_string exn
    in
    eprintf "%s: %s\n%!" (color Red "Error") s;
    exit 1
