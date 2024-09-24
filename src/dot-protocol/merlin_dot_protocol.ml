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

open Merlin_utils.Std
open Merlin_utils.Std.Result

module Directive = struct
  type include_path =
    [ `B of string
    | `S of string
    | `BH of string
    | `SH of string
    | `CMI of string
    | `CMT of string
    | `INDEX of string ]

  type no_processing_required =
    [ `EXT of string list
    | `FLG of string list
    | `STDLIB of string
    | `SOURCE_ROOT of string
    | `UNIT_NAME of string
    | `WRAPPING_PREFIX of string
    | `SUFFIX of string
    | `READER of string list
    | `EXCLUDE_QUERY_DIR
    | `USE_PPX_CACHE
    | `UNKNOWN_TAG of string ]

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
  type t = Csexp.t = Atom of string | List of t list

  let atoms_of_strings = List.map ~f:(fun s -> Atom s)

  let strings_of_atoms =
    List.filter_map ~f:(function
      | Atom s -> Some s
      | _ -> None)

  let rec to_string = function
    | Atom s -> s
    | List l ->
      String.concat ~sep:" "
        (List.concat [ [ "(" ]; List.map ~f:to_string l; [ ")" ] ])

  let to_directive sexp =
    match sexp with
    | List [ Atom tag; Atom value ] -> begin
      match tag with
      | "S" -> `S value
      | "B" -> `B value
      | "SH" -> `SH value
      | "BH" -> `BH value
      | "CMI" -> `CMI value
      | "CMT" -> `CMT value
      | "INDEX" -> `INDEX value
      | "STDLIB" -> `STDLIB value
      | "SOURCE_ROOT" -> `SOURCE_ROOT value
      | "UNIT_NAME" -> `UNIT_NAME value
      | "WRAPPING_PREFIX" -> `WRAPPING_PREFIX value
      | "SUFFIX" -> `SUFFIX value
      | "ERROR" -> `ERROR_MSG value
      | "FLG" ->
        (* This means merlin asked dune 2.6 for configuration.
           But the protocole evolved, only dune 2.8 should be used *)
        `ERROR_MSG "No .merlin file found. Try building the project."
      | tag -> `UNKNOWN_TAG tag
    end
    | List [ Atom tag; List l ] ->
      let value = strings_of_atoms l in
      begin
        match tag with
        | "EXT" -> `EXT value
        | "FLG" -> `FLG value
        | "READER" -> `READER value
        | tag -> `UNKNOWN_TAG tag
      end
    | List [ Atom "EXCLUDE_QUERY_DIR" ] -> `EXCLUDE_QUERY_DIR
    | List [ Atom "USE_PPX_CACHE" ] -> `USE_PPX_CACHE
    | _ -> `ERROR_MSG "Unexpected output from external config reader"

  let from_directives (directives : Directive.Processed.t list) =
    let f t =
      let tag, body =
        let single s = [ Atom s ] in
        match t with
        | `B s -> ("B", single s)
        | `S s -> ("S", single s)
        | `BH s -> ("BH", single s)
        | `SH s -> ("SH", single s)
        | `CMI s -> ("CMI", single s)
        | `CMT s -> ("CMT", single s)
        | `INDEX s -> ("INDEX", single s)
        | `SOURCE_ROOT s -> ("SOURCE_ROOT", single s)
        | `UNIT_NAME s -> ("UNIT_NAME", single s)
        | `WRAPPING_PREFIX s -> ("WRAPPING_PREFIX", single s)
        | `EXT ss -> ("EXT", [ List (atoms_of_strings ss) ])
        | `FLG ss -> ("FLG", [ List (atoms_of_strings ss) ])
        | `STDLIB s -> ("STDLIB", single s)
        | `SUFFIX s -> ("SUFFIX", single s)
        | `READER ss -> ("READER", [ List (atoms_of_strings ss) ])
        | `EXCLUDE_QUERY_DIR -> ("EXCLUDE_QUERY_DIR", [])
        | `USE_PPX_CACHE -> ("USE_PPX_CACHE", [])
        | `UNKNOWN_TAG tag ->
          ("ERROR", single @@ Printf.sprintf "Unknown tag in .merlin: %s" tag)
        | `ERROR_MSG s -> ("ERROR", single s)
      in
      List (Atom tag :: body)
    in
    List (List.map ~f directives)
end

type read_error = Unexpected_output of string | Csexp_parse_error of string

type command = File of string | Halt | Unknown

module type S = sig
  type 'a io
  type in_chan
  type out_chan

  (** [read] reads one csexp from the channel and returns the list of
      directives it represents *)
  val read :
    in_chan -> (directive list, read_error) Merlin_utils.Std.Result.t io

  val write : out_chan -> directive list -> unit io

  module Commands : sig
    val read_input : in_chan -> command io

    val send_file : out_chan -> string -> unit io

    val halt : out_chan -> unit io
  end
end

module Make (IO : sig
  type 'a t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end
end) (Chan : sig
  type in_chan
  type out_chan

  val read : in_chan -> (Csexp.t, string) result IO.t

  val write : out_chan -> Csexp.t -> unit IO.t
end) =
struct
  type 'a io = 'a IO.t
  type in_chan = Chan.in_chan
  type out_chan = Chan.out_chan

  module Commands = struct
    let read_input chan =
      let open Sexp in
      let open IO.O in
      let+ input = Chan.read chan in
      match input with
      | Ok (List [ Atom "File"; Atom path ]) -> File path
      | Ok (Atom "Halt") -> Halt
      | Ok _ -> Unknown
      | Error _ -> Halt

    let send_file chan path =
      Chan.write chan Sexp.(List [ Atom "File"; Atom path ])

    let halt chan = Chan.write chan (Sexp.Atom "Halt")
  end

  let read chan =
    let open IO.O in
    let+ res = Chan.read chan in
    match res with
    | Ok (Sexp.List directives) -> Ok (List.map directives ~f:Sexp.to_directive)
    | Ok sexp ->
      let msg =
        Printf.sprintf "A list of directives was expected, instead got: \"%s\""
          (Sexp.to_string sexp)
      in
      Error (Unexpected_output msg)
    | Error msg -> Error (Csexp_parse_error msg)

  let write out_chan (directives : directive list) =
    directives |> Sexp.from_directives |> Chan.write out_chan
end

module Blocking =
  Make
    (struct
      type 'a t = 'a
      module O = struct
        let ( let+ ) x f = f x
      end
    end)
    (struct
      type in_chan = in_channel
      type out_chan = out_channel
      let read = Csexp.input
      let write = Csexp.to_channel
    end)
