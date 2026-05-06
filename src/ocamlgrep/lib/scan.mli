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

(** [incremental_search paths handler] scans the project starting
    from the search root embedded in [paths]. Each time a finding or a
    warning is created, the [handler] function is called. *)
val incremental_search : Paths.t -> (event -> unit) -> string -> unit

(** Wrapper around [incremental_search] that returns the results as a list
    at the end instead of incrementally. *)
val search : Paths.t -> string -> event list
