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

(* Highlight the substring [s.[lo..hi)] in red. Out-of-range indices
   are clamped silently — a stale cmt could in principle produce them
   even after the digest check, and crashing the renderer would be a
   poor failure mode. *)
let highlight_range line lo hi =
  let n = String.length line in
  let lo = max 0 (min n lo) in
  let hi = max lo (min n hi) in
  if lo = hi then line
  else
    String.sub line 0 lo
    ^ color Red "%s" (String.sub line lo (hi - lo))
    ^ String.sub line hi (n - hi)

(* Format A: a header line giving the precise location, followed by
   the matched source lines with an OCaml-compiler-style [N |] gutter.

       foo.ml:5:10-22:
       5 |   let x = List.length xs

       foo.ml:6:10-8:9:
       6 |   let y =
       7 |     foo bar
       8 |       baz

   The header is unambiguous so consecutive findings need no
   separator between them. *)
let print_finding (f : Scan.finding) =
  let file = color Green "%s" f.source in
  let header =
    if f.start_line = f.end_line then
      sprintf "%s:%d:%d-%d:" file f.start_line f.start_col f.end_col
    else
      sprintf "%s:%d:%d-%d:%d:" file
        f.start_line f.start_col f.end_line f.end_col
  in
  print_endline header;
  let gutter_width = String.length (string_of_int f.end_line) in
  List.iteri
    (fun i line ->
      let lineno = f.start_line + i in
      let lo = if lineno = f.start_line then f.start_col else 0 in
      let hi = if lineno = f.end_line then f.end_col else String.length line in
      printf "%s | %s\n" (color Yellow "%*d" gutter_width lineno)
        (highlight_range line lo hi))
    f.lines;
  (* Flush so streamed output is interleaved with stderr warnings in order. *)
  printf "%!"

let handle_event (ev : Scan.event) =
  match ev with
  | Scan_file _path -> ()
  | Warning msg -> warn msg
  | Finding finding -> print_finding finding

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
