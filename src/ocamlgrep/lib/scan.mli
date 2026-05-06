(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi *)
(**
   Ocamlgrep library - type-aware search for OCaml code patterns
*)

(** Matching snippet of code. This will change.
    It currently only shows the first line of the match.
    TODO: include multiline match info
*)
type finding = {
  source: string;
  i: int;
  c1: int;
  c2: int;
  s: string;
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
