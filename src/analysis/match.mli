(* This file is part of merlin.
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)

(**
   Type-aware structural search for OCaml code.

   This module implements the pattern matching engine behind the
   [ocamlmerlin single ocamlgrep -query <pattern>] subcommand.  It
   searches the typed AST (cmt files) of a Dune project for
   sub-expressions matching a given pattern.

   {2 Quick start}

   Run [dune build @check] to make sure the project's cmt files are
   up to date, then:

   {[
     ocamlmerlin single ocamlgrep -query 'List.filter __ __' < /dev/null
   ]}

   {2 Pattern syntax}

   A pattern is any valid OCaml expression.  The special identifiers
   below carry additional meaning:

   {3 Wildcards}

   [__] (two underscores) matches any expression or record field
   (a "metavariable" in the sense of coccinelle/semgrep).

   [__1], [__2], ... are {e numbered} metavariables: each occurrence
   of the same number must match the same (structurally equal)
   expression.  For example,
   {[
     match __ with Some __1 -> Some __1 | _ -> None
   ]}
   only matches branches that return their [Some] payload unchanged.

   {3 Identifiers}

   An identifier or constructor in the pattern is matched as a
   {e suffix} of the fully-qualified path in the typed tree:
   [f] matches [Module.f], [M.f] matches [Outer.M.f].  This makes
   patterns robust to [open] declarations in the matched code.

   {3 Function application}

   In a function application, trailing arguments may be omitted; the
   pattern matches any call that supplies at least the listed
   arguments.  Optional arguments have two special forms:

   - [foo ?arg:PRESENT] — the optional argument must be supplied
   - [foo ?arg:MISSING] — the optional argument must be omitted

   {3 Type constraints}

   [(e : t)] matches any expression that matches [e] and whose
   inferred type unifies with [t].  The wildcard [__] is allowed in
   [t], e.g. [(__ : __ list)].

   {3 Match and record expressions}

   Clauses in a [match] or [try] expression, and fields in a record
   expression, are matched as a {e set}: order does not matter, and a
   single pattern clause may match multiple program clauses.

   {3 Field access}

   [e.lid] matches both field reads ([x.lid]) and writes
   ([x.lid <- _]).  The form [__.id] additionally matches record
   patterns [{...; id; ...}], making it a universal "find all uses of
   field [id]" pattern.

   {2 Examples}

   {[
     (* Every call to List.filter: *)
     List.filter

     (* Casts from floatarray that go through float array: *)
     (__ (__ : floatarray) : float array)

     (* The classic list-reversal antipattern: *)
     List.rev __ @ __

     (* Identity-like Some/None pattern: *)
     match __ with None -> __ | Some __1 -> Some __1

     (* Composed map and fold: *)
     List.fold_left __ __ (List.map __ __)

     (* Float max with stdlib: *)
     Stdlib.max (__ : float) __
   ]}

   {2 Output format}

   Results are returned as JSON by the merlin subcommand:

   {v
   ocamlmerlin single ocamlgrep -query <pattern> < /dev/null
   v}

   Each finding in the [findings] array has the shape:

   {v
   { "file":  "src/foo.ml",
     "start": { "line": 5, "col": 10 },
     "end":   { "line": 5, "col": 22 },
     "lines": [ "  let x = List.length xs in" ] }
   v}

   Line numbers are 1-based; column offsets are 0-based (character
   offset from the start of the line).  [lines] contains every source
   line from [start.line] to [end.line] inclusive, in order.
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
  Parsetree.expression -> Cmt_format.cmt_infos -> Location.t list

(** [search query cmt ~source ~src_lines] calls {!search_cmt}
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
val search :
  Parsetree.expression ->
  Cmt_format.cmt_infos -> source:string -> src_lines:string array ->
  finding list
