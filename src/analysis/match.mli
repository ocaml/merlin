(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)
(**
   Match a pattern against a program
*)

exception Cannot_parse_type of exn

(** A region of source code that matched a query pattern.
    The user-friendly source path lives in [loc.loc_start.pos_fname]. *)
type finding = {
  loc : Location.t;
  lines : string list;
    (** Source lines spanned by [loc], i.e. lines
        [loc.loc_start.pos_lnum .. loc.loc_end.pos_lnum] inclusive,
        in order.  Always non-empty. *)
}

(** [parse_query s] parses the string [s] as a single OCaml expression
    to be used as a pattern in {!search_cmt} or {!search_findings}.
    Raises [Failure] with a human-readable message if [s] is not a
    valid OCaml expression. *)
val parse_query : string -> Parsetree.expression

(** [search_cmt cmt_data query] scans the typed tree in [cmt_data]
    for sub-expressions matching the pattern [query] and returns the
    list of matching source locations.

    [query] is a parsed expression without inferred type information
    (as returned by {!parse_query}). The program tree is typed.

    Exceptions will be raised, including [Cannot_parse_type]. *)
val search_cmt :
  Cmt_format.cmt_infos -> Parsetree.expression -> Location.t list

(** [search_findings query cmt ~source ~src_lines] calls {!search_cmt}
    and converts each matching location into a {!finding}, clamping
    line numbers to the actual extent of the file and overriding
    [pos_fname] with [source] so locations carry the user-friendly
    project-relative path rather than the compiler-recorded build path.

    This function has exactly the signature expected by
    {!Merlin_project.Scan.incremental_search} when partially applied
    to [query]:
    {[
      let search = Match.search_findings expr in
      Scan.incremental_search paths cmt_files handler search
    ]} *)
val search_findings :
  Parsetree.expression ->
  Cmt_format.cmt_infos -> source:string -> src_lines:string array ->
  finding list
