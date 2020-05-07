(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2019  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std
open Std.Result

module Directive = struct
  type include_path =
    [ `B of string | `S of string | `CMI of string | `CMT of string ]

  type no_processing_required =
    [ `EXT of string list
    | `FLG of string
    | `STDLIB of string
    | `SUFFIX of string
    | `READER of string list
    | `EXCLUDE_QUERY_DIR ]

  module Processed = struct
    type acceptable_in_input = [ include_path | no_processing_required ]

    type t = [ acceptable_in_input | `ERROR_MSG of string ]
  end

  module Raw = struct
    type t =
      [ Processed.acceptable_in_input
      | `PKG of string list
      | `FINDLIB of string
      | `FINDLIB_PATH of string
      | `FINDLIB_TOOLCHAIN of string ]
  end
end

type directive = Directive.Processed.t

module Sexp = struct
  type t = Atom of string | List of t list

  exception Bad_directive of string * string
  exception Unexpected of string

  let atoms_of_strings = List.map ~f:(fun s -> Atom s)

  let strings_of_atoms =
    List.filter_map ~f:(function Atom s -> Some s | _ -> None)

  let to_directive sexp =
    try
      Ok
        ( match sexp with
        | List [ Atom tag; Atom value ] ->
          begin match tag with
            | "S" -> `S value
            | "B" -> `B value
            | "CMI" -> `CMI value
            | "CMT" -> `CMT value
            | "FLG" -> `FLG value
            | "STDLIB" -> `STDLIB value
            | "SUFFIX" -> `SUFFIX value
            | "ERROR" -> `ERROR_MSG value
            | _ -> raise (Bad_directive (tag, value))
          end
        | List [ Atom tag; List l ] -> (
            let value = strings_of_atoms l in
            match tag with
            | "EXT" -> `EXT value
            | "READER" -> `READER value
            | _ -> raise (Bad_directive (tag, String.concat ~sep:" " value)) )
        | List [ Atom "EXCLUDE_QUERY_DIR" ] -> `EXCLUDE_QUERY_DIR
        | _ -> raise (Unexpected "Unexpect s-expression form") )
    with
    | Bad_directive (tag, value) ->
        let msg =
          Printf.sprintf "Unknown or ill-formed directive \"%s %s\"" tag value
        in
        Error msg
    | Unexpected e -> Error e

  let from_directives (directives : Directive.Processed.t list) =
    let f t =
      let tag, body =
        let single s = [ Atom s ] in
        match t with
        | `B s -> ("B", single s)
        | `S s -> ("S", single s)
        | `CMI s -> ("CMI", single s)
        | `CMT s -> ("CMT", single s)
        | `EXT ss -> ("EXT", [ List (atoms_of_strings ss) ])
        | `FLG s -> ("FLG", single s)
        | `STDLIB s -> ("STDLIB", single s)
        | `SUFFIX s -> ("SUFFIX", single s)
        | `READER ss -> ("READER", [ List (atoms_of_strings ss) ])
        | `EXCLUDE_QUERY_DIR -> ("EXCLUDE_QUERY_DIR", [])
        | `ERROR_MSG s -> ("ERROR", single s)
      in
      List (Atom tag :: body)
    in
    List (List.map ~f directives)
end

module Csexp = Csexp.Make (Sexp)

module Commands = struct
  type t = File of string | Halt | Unknown

  let read_input in_channel =
    let open Sexp in
    match Csexp.input in_channel with
    | Ok (List [Atom "File"; Atom path]) -> File path
    | Ok (Atom "Halt") -> Halt
    | Ok _ -> Unknown
    | Error _msg -> Halt

  let make_f path =
    Sexp.(List [Atom "File"; Atom path])
    |> Csexp.to_string
end


let read ~in_channel =
  let str = input_line in_channel in
  match Csexp.parse_string str with
  | Ok (Sexp.List directives) ->
      List.rev
        (List.filter_map directives ~f:(fun dir ->
             match Sexp.to_directive dir with
             | Ok dir -> Some dir
             | Error msg ->
                 Logger.notify ~section:"CSEXP parse error"
                   "%s in\n%s" msg str;
                 None))
  | Ok _  ->
      Logger.notify ~section:"CSEXP parse error" "Parser expected a toplevel list";
      []
  | Error (_, s) ->
      Logger.notify ~section:"CSEXP parse error" "Bad CSEXP \"%s\" (in \"%s\"" s str;
      []

let write ~out_channel (directives : directive list) =
  directives |> Sexp.from_directives |> Csexp.to_channel out_channel
