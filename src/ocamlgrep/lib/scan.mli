(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)
(**
   Ocamlgrep library - type-aware search for OCaml code patterns
*)

(** A region of source code matched by a query. Lines and columns are
    1-based and 0-based respectively, matching [Lexing.position]. *)
type finding = {
  source: string;
  start_line: int;
  start_col: int;
  end_line: int;
  end_col: int;
  lines: string list;
    (** The source lines from [start_line] through [end_line] inclusive,
        in order. Always non-empty. *)
}

type event =
  | Scan_file of string (** emitted when a new source file is about to be
                            scanned *)
  | Finding of finding (** found a matching region of code *)
  | Warning of string (** a warning message, possibly containing line breaks *)

(** [incremental_search paths cmt_files handler query] processes each
    cmt file in [cmt_files] and matches its typed tree against the
    pattern [query]. The caller is responsible for enumerating
    [cmt_files] (e.g. via {!Paths.collect_cmt_files}); this function
    does no filesystem traversal of its own. [paths] is used to
    resolve cmt-recorded source paths to project-relative display
    paths and to locate preprocessed sources for digest checks.

    Each time a finding or a warning is created, [handler] is called. *)
val incremental_search :
  Paths.t -> string list -> (event -> unit) -> string -> unit

(** Wrapper around [incremental_search] that returns the results as a list
    at the end instead of incrementally. *)
val search : Paths.t -> string list -> string -> event list
