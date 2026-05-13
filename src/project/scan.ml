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
   source lines array.  Each result is emitted as a [Finding] event.
   Returns true if the cmt file was found, false if it was missing. *)
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
    (* [source] is the project-relative path used in findings (pos_fname).
       [abs_source] is the absolute path used for all filesystem operations,
       so they work regardless of the process's CWD. *)
    let source, abs_source =
      if Filename.check_suffix source ".pp.ml" then
        ( Filename.chop_suffix source ".pp.ml" ^ ".ml",
          Paths.in_build_dir paths source )
      else
        let rel =
          drop_prefix ~prefix:paths.project_relative_search_root source
        in
        (rel, Filename.concat paths.project_root rel)
    in
    handle_event (Scan_file source);
    if not (Sys.file_exists abs_source) then true
    else if digest <> Digest.file abs_source then begin
      handle_event
        (Warning
           (sprintf "%s does not correspond to %s (ignoring)" cmt_path abs_source));
      true
    end
    else begin
      let src_lines = Array.of_list (read_lines abs_source) in
      (match search cmt ~source ~src_lines with
       | exception exn ->
         handle_event
           (Warning
              (Format.asprintf "error while analysing %s: %a" cmt_path
                 Location.report_exception exn))
       | results ->
         List.iter (fun r -> handle_event (Finding r)) results);
      true
    end
  | { cmt_sourcefile = None; _ } | { cmt_source_digest = None; _ } -> true
  | exception Cmt_format.Error (Cmt_format.Not_a_typedtree _) ->
    handle_event (Warning (sprintf "error reading cmt file: %s" cmt_path));
    true
  | exception Sys_error _ ->
    (* cmt file does not exist yet — project needs to be (re)built *)
    false

let incremental_search
    (paths : Paths.t)
    (cmt_files : string list)
    (handle_event : 'a event -> unit)
    (search : Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list) : unit =
  let total = List.length cmt_files in
  let found =
    List.fold_left
      (fun acc cmt_path ->
        if process_one_cmt paths handle_event search cmt_path then acc + 1
        else acc)
      0
      cmt_files
  in
  if found < total then begin
    let missing = total - found in
    let pct = (found * 100) / total in
    handle_event
      (Warning
         (sprintf
            "%d/%d cmt files found (%d%% coverage); \
             %d missing — run 'dune build @check' to generate them"
            found total pct missing))
  end

let search paths cmt_files fn =
  let events = ref [] in
  let handle_event ev = events := ev :: !events in
  incremental_search paths cmt_files handle_event fn;
  List.rev !events
