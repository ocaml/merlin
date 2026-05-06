(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi *)
(**
   Match a pattern against a program
*)

exception Cannot_parse_type of exn

(** [search_cmt cmt_data query] scans the [query] expression against
    the parsed contents of a cmt file [cmt_data] and returns a list
    of matching locations.

    The query is a parsed expression without inferred type information.
    The program is a typed tree.

    Exceptions will be raised, including [Cannot_parse_type].
*)
val search_cmt :
  Cmt_format.cmt_infos -> Parsetree.expression -> Warnings.loc list
