(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

type t = {valid: bool; loc: Location.t; text: string; sub: (string * Location.t) list; where:string}

let format ~valid ~where ?(loc=Location.none) ?(sub=[]) text =
  loc, {valid; loc; text; sub; where}

let of_suberror {Location. err_loc; msg} = (msg, err_loc)

let strict_of_exn exn =
  let valid = true in
  match Location.error_of_exn exn with
  | Some {Location. err_loc = loc; sub; msg} ->
    let where = match exn with
      | Syntaxerr.Escape_error _ | Syntaxerr.Error _ -> "parser"
      | _ -> "type" in
    Some (format ~valid ~where ~loc ~sub:(List.map ~f:of_suberror sub) msg)
  | None ->
    match exn with
    | Parsing_aux.Warning (loc, msg) ->
      Some (format ~valid:true ~where:"warning" ~loc msg)
    | Raw_parser.Error ->
      Some (format ~valid:false ~where:"parser" "Parse error")
    | Findlib.No_such_package (pkg,msg) ->
      Some (format ~valid:true ~where:"env"
              (Printf.sprintf "Package not found %S (%s)" pkg msg))
    | Error_classifier.Error c ->
      let loc = Error_classifier.loc c in
      let msg = Error_classifier.classify c in
      Some (format ~valid:true ~where:"parser" ~loc msg)
    | Env.Error error ->
       let ppf, to_string = Format.to_string () in
       Env.report_error ppf error;
       Some (format ~valid:true ~where:"env" (to_string ()))
    | _ -> None

let null_loc =
  let z = {Lexing. pos_fname = ""; pos_bol = 0; pos_lnum = 1; pos_cnum = 0} in
  {Location. loc_start = z; loc_end = z; loc_ghost = true}

let of_exn exn = match strict_of_exn exn with
  | Some j -> j
  | None -> format ~valid:false ~where:"unknown" ~loc:null_loc
              (Printexc.to_string exn)

let error_catcher = strict_of_exn

let flood_barrier ?(threshold=10) errors =
  let dam = Hashtbl.create 17 in
  let f reported error =
    try
      let nb_occurences = Hashtbl.find dam error.text in
      if nb_occurences >= threshold then
        reported
      else (
        Hashtbl.replace dam error.text (nb_occurences + 1);
        error :: reported
      )
    with Not_found ->
      Hashtbl.add dam error.text 1;
      error :: reported
  in
  List.(rev @@ fold_left ~f ~init:[] errors)
