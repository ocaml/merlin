(**
   Build-system-agnostic framework for scanning a project's cmt files.

   This module handles the mechanics of reading cmt files, verifying
   source digests, mapping compiler-internal paths back to
   project-relative display paths, and emitting structured events.
   The actual search logic and the finding type are supplied by the
   caller.

   Typical usage:

   {[
     (* 1. Locate the project and initialise Load_path. *)
     let paths = Paths.init () |> Result.get_ok in

     (* 2. Enumerate cmt files (e.g. via Dune_workspace). *)
     let cmt_files = ... in

     (* 3. Supply a search function and collect results. *)
     let handle_event = function
       | Scan.Finding f -> ...
       | Scan.Warning msg -> ...
       | Scan.Scan_file _ -> ()
     in
     Scan.incremental_search paths cmt_files handle_event my_search
   ]}
*)

(** Events emitted during a scan.  The type parameter ['a] is the
    finding type chosen by the caller — for example
    [Merlin_analysis.Expr_search.finding]. *)
type 'a event =
  | Scan_file of string
      (** Emitted when scanning of a new source file begins. The
          string is the user-friendly, project-relative source path. *)
  | Finding of 'a
      (** A result produced by the search function for one cmt file. *)
  | Warning of string
      (** A non-fatal diagnostic, possibly containing line breaks. *)

(** [incremental_search acc paths cmt_files handler search] iterates over
    [cmt_files], reads each one, verifies its source digest, and calls
    [search cmt ~source ~src_lines] where:

    - [acc] is the initial value of the result accumulator,
    - [cmt] is the parsed cmt data,
    - [source] is the user-friendly, project-relative path to the
      source file (suitable for use as [pos_fname] in locations), and
    - [src_lines] is the content of the source file as an array of
      lines (suitable for extracting context around matches).

    Each element of the list returned by [search] is emitted as a
    [Finding] event.  Exceptions raised by [search] are caught and
    emitted as [Warning] events so that a single bad cmt file does not
    abort the whole scan.

    The caller is responsible for enumerating [cmt_files]
    (e.g. via {!Dune_workspace.local_cmt_files}); this function does
    no filesystem traversal of its own.

    [paths] is used to resolve cmt-recorded source paths to
    project-relative display paths and to locate preprocessed sources
    for digest checks. *)
val incremental_search :
  'acc ->
  Paths.t ->
  string list ->
  ('acc -> 'a event -> 'acc) ->
  (Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list) ->
  'acc

(** Wrapper around [incremental_search] that accumulates events into a
    list returned at the end instead of calling a handler
    incrementally. *)
val search :
  Paths.t ->
  string list ->
  (Cmt_format.cmt_infos -> source:string -> src_lines:string array -> 'a list) ->
  'a event list
