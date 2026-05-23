(* Project-wide scan for cmt files *)

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
    (acc : 'acc)
    (paths : Paths.t)
    handle_event
    (search : Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list)
    cmt_path : ('acc, unit) result =
  (* Read from the cache but don't add new entries to it since it would grow
     as big as the whole project. *)
  match (Cmt_cache.read ~read_only:true cmt_path).cmt_infos with
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
    (* Resolve [source] (from cmt_sourcefile) to an absolute path we can read.
       Returns [(project_rel, abs, skip_digest)] where skip_digest is true when
       we are reading the human-written .ml instead of the preprocessed .pp.ml
       that was actually compiled (so the digest check does not apply). *)
    let resolve_source source =
      let build_prefix = paths.build_source_root ^ "/" in
      (* Strip any build-dir prefix from an absolute path. *)
      let strip_build abs =
        let stripped = drop_prefix ~prefix:build_prefix abs in
        if String.length stripped < String.length abs then Some stripped
        else None
      in
      if Filename.check_suffix source ".pp.ml" then begin
        (* The cmt was compiled from a ppx-preprocessed file.  The .pp.ml in
           _build/ may be binary (OCaml binary AST).  Find the original .ml in
           the project source tree instead and skip the digest check. *)
        let rel_pp =
          let r = drop_prefix ~prefix:paths.project_relative_search_root source in
          let abs = Filename.concat paths.project_root r in
          match strip_build abs with
          | Some s -> s          (* was build-relative *)
          | None ->
            drop_prefix ~prefix:(paths.project_root ^ "/") abs
        in
        let rel_ml = Filename.chop_suffix rel_pp ".pp.ml" ^ ".ml" in
        (rel_ml, Filename.concat paths.project_root rel_ml, true)
      end else begin
        let rel = drop_prefix ~prefix:paths.project_relative_search_root source in
        let abs = Filename.concat paths.project_root rel in
        let abs_source =
          match strip_build abs with
          | Some project_rel -> Filename.concat paths.project_root project_rel
          | None -> abs
        in
        let source_rel =
          drop_prefix ~prefix:(paths.project_root ^ "/") abs_source
        in
        (source_rel, abs_source, false)
      end
    in
    let source, abs_source, skip_digest = resolve_source source in
    let acc = handle_event acc (Scan_file source) in
    if not (Sys.file_exists abs_source) then Ok acc
    else if (not skip_digest) && digest <> Digest.file abs_source then begin
      let acc =
        handle_event acc
          (Warning
             (sprintf
                "%s does not correspond to %s (ignoring)"
                cmt_path abs_source))
      in
      Ok acc
    end
    else begin
      let src_lines = Array.of_list (read_lines abs_source) in
      let acc =
        match search cmt ~source ~src_lines with
        | exception exn ->
            handle_event acc
              (Warning
                 (Format.asprintf "error while analysing %s: %a" cmt_path
                    Location.report_exception exn))
        | results ->
            List.fold_left
              (fun acc r -> handle_event acc (Finding r)) acc results
      in
      Ok acc
    end
  | { cmt_sourcefile = None; _ } | { cmt_source_digest = None; _ } -> Ok acc
  | exception Cmt_format.Error (Cmt_format.Not_a_typedtree _) ->
      let acc =
        handle_event acc
          (Warning (sprintf "error reading cmt file: %s" cmt_path))
      in
      Ok acc
  | exception Sys_error _ ->
      (* cmt file does not exist yet - project needs to be (re)built *)
      Error ()

let incremental_search
    (acc : 'acc)
    (paths : Paths.t)
    (cmt_files : string list)
    (handle_event : 'acc -> 'a event -> 'acc)
    (search : Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list) : 'acc =
  let total = List.length cmt_files in
  let acc, found =
    List.fold_left
      (fun (acc, found) cmt_path ->
        match process_one_cmt acc paths handle_event search cmt_path with
          | Ok acc -> (acc, found + 1)
          | Error () -> (acc, found)
      )
      (acc, 0)
      cmt_files
  in
  if found < total then begin
    let missing = total - found in
    let pct = (found * 100) / total in
    handle_event acc
      (Warning
         (sprintf
            "%d/%d cmt files found (%d%% coverage); \
             %d missing — run 'dune build @check' to generate them"
            found total pct missing))
  end
  else
    acc
