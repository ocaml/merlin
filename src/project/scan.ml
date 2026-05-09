(* This file is part of merlin.
   See the attached LICENSE file.
   Copyright (C) 2000-2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)

open Printf

type 'a event =
  | Scan_file of string
  | Finding of 'a
  | Warning of string

(* This is used to make a path relative to another.
   Similar to Fpath.relativize. *)
let drop_prefix ~prefix s =
  if String.starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else
    s

let read_lines fn =
  String.split_on_char '\n' (In_channel.with_open_text fn In_channel.input_all)

(*** Structured search ***)

(* Process a single cmt file: read it, digest-check the source, then
   call [search] with the cmt data, the resolved source path, and the
   source lines array.  Each result is emitted as a [Finding] event. *)
let process_one_cmt
    (paths : Paths.t)
    handle_event
    (search : Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list)
    cmt_path =
  match Cmt_format.read_cmt cmt_path with
  | { Cmt_format.cmt_sourcefile = Some source;
      cmt_source_digest = Some digest;
      _
    } as cmt ->
    (* source     = user-friendly path to the source file, relative to
                    the search root (typically cwd)
       pp_source = any valid path to the preprocessed ml file *)
    let source, pp_source =
      if Filename.check_suffix source ".pp.ml" then
        ( Filename.chop_suffix source ".pp.ml" ^ ".ml",
          Paths.in_build_dir paths source )
      else
        let source =
          drop_prefix ~prefix:paths.project_relative_search_root source
        in
        (source, source)
    in
    handle_event (Scan_file source);
    if not (Sys.file_exists pp_source) then ()
    else if digest <> Digest.file pp_source then
      handle_event
        (Warning
           (sprintf "%s does not correspond to %s (ignoring)" cmt_path pp_source))
    else begin
      let src_lines = Array.of_list (read_lines source) in
      match search cmt ~source ~src_lines with
      | exception exn ->
        handle_event
          (Warning
             (Format.asprintf "error while analysing %s: %a" cmt_path
                Location.report_exception exn))
      | results ->
        List.iter (fun r -> handle_event (Finding r)) results
    end
  | { cmt_sourcefile = None; _ } | { cmt_source_digest = None; _ } -> ()
  | exception Cmt_format.Error (Cmt_format.Not_a_typedtree _) ->
    failwith ("error reading cmt file: " ^ cmt_path)

let incremental_search
    (paths : Paths.t)
    (cmt_files : string list)
    (handle_event : 'a event -> unit)
    (search : Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list) : unit =
  List.iter (process_one_cmt paths handle_event search) cmt_files

let search paths cmt_files fn =
  let events = ref [] in
  let handle_event ev = events := ev :: !events in
  incremental_search paths cmt_files handle_event fn;
  List.rev !events
