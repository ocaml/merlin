(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open Parsetree
open Std

let default_loc = function
  | None -> Location.none
  | Some loc -> loc

let mkoptloc opt x =
  match opt with
  | None -> Location.mknoloc x
  | Some l -> Location.mkloc x l

let app a b =
  let loc =
    if a.pexp_loc.Location.loc_ghost
    then {b.pexp_loc with Location.loc_ghost = true}
    else b.pexp_loc
  in
  Ast_helper.Exp.apply ~loc a [Raw_compat.Parsetree.arg_label_of_str "", b]

let pat_app f (pat,expr) = pat, app f expr

let prim_ident prim = Longident.parse ("_." ^ prim)
let prim ?(ghost=true) prim =
  let open Location in
  let ident = mknoloc (prim_ident prim) in
  let ident = if ghost
    then ident
    else {ident with loc = {ident.loc with loc_ghost = false}}
  in
  Ast_helper.Exp.ident ~loc:ident.loc ident

let any_val' = prim "Any.val'"

(* Lwt extension *)
module Lwt = struct
  let un_lwt = prim "Lwt.un_lwt"
  let to_lwt = prim "Lwt.to_lwt"
  let in_lwt = prim "Lwt.in_lwt"
  let unit_lwt = prim "Lwt.unit_lwt"
  let un_stream = prim "Lwt.un_stream"
  let finally_ = prim "Lwt.finally'"
  let raise_lwt_ = prim_ident "Lwt.raise_lwt'"
end

(* MetaOCaml support *)
module Meta = struct
  let prim_code = prim "Meta.code"
  let prim_uncode = prim "Meta.uncode"

  let code loc_start loc_end expr =
    let loc = {expr.pexp_loc with Location. loc_start; loc_end} in
    Ast_helper.Exp.apply ~loc prim_code [Raw_compat.no_label, expr]

  let uncode loc_start loc_end expr =
    let loc = {expr.pexp_loc with Location. loc_start; loc_end} in
    Ast_helper.Exp.apply ~loc prim_uncode [Raw_compat.no_label, expr]
end
