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

open Std

let {Logger. log} = Logger.for_section "context"

type t =
  | Constructor of Types.constructor_description
    (* We attach the constructor description here so in the case of
      disambiguated constructors we actually directly look for the type
      path (cf. #486, #794). *)
  | Expr
  | Label of Types.label_description (* Similar to constructors. *)
  | Module_path
  | Module_type
  | Patt
  | Type
  | Unknown

let to_string = function
  | Constructor cd -> Printf.sprintf "constructor %s" cd.cstr_name
  | Expr -> "expression"
  | Label lbl -> Printf.sprintf "record field %s" lbl.lbl_name
  | Module_path -> "module path"
  | Module_type -> "module type"
  | Patt -> "pattern"
  | Type -> "type"
  | Unknown -> "unknown"

(* Distinguish between "Mo[d]ule.Constructor" and "Module.Cons[t]ructor" *)
let cursor_on_constructor_name ~cursor:pos
      ~cstr_token:{ Asttypes.loc; txt = lid } cd =
  match lid with
  | Longident.Lident _ -> true
  | _ ->
    let end_offset = loc.loc_end.pos_cnum in
    let constr_pos =
      { loc.loc_end
        with pos_cnum = end_offset - String.length cd.Types.cstr_name }
    in
    Lexing.compare_pos pos constr_pos >= 0

let inspect_pattern ~pos ~lid p =
  let open Typedtree in
  log ~title:"inspect_context" "%a" Logger.fmt
    (fun fmt -> Format.fprintf fmt "current pattern is: %a"
                  (Printtyped.pattern 0) p);
  match p.pat_desc with
  | Tpat_any when Longident.last lid = "_" -> None
  | Tpat_var (_, str_loc) when (Longident.last lid) = str_loc.txt ->
    None
  | Tpat_alias (_, _, str_loc)
    when (Longident.last lid) = str_loc.txt ->
    (* Assumption: if [Browse.enclosing] stopped on this node and not on the
      subpattern, then it must mean that the cursor is on the alias. *)
    None
  | Tpat_construct (lid_loc, cd, _)
    when cursor_on_constructor_name ~cursor:pos ~cstr_token:lid_loc cd
        && (Longident.last lid) = (Longident.last lid_loc.txt) ->
    (* Assumption: if [Browse.enclosing] stopped on this node and not on the
      subpattern, then it must mean that the cursor is on the constructor
      itself.  *)
      Some (Constructor cd)
  | _ ->
    Some Patt

let inspect_expression ~pos ~lid e : t =
  match e.Typedtree.exp_desc with
  | Texp_construct (lid_loc, cd, _)
    when cursor_on_constructor_name ~cursor:pos ~cstr_token:lid_loc cd
        && (Longident.last lid) = (Longident.last lid_loc.txt) ->
    Constructor cd
  | _ ->
    Expr

let inspect_browse_tree browse lid pos : t option =
  match Mbrowse.enclosing pos browse with
  | [] ->
    log ~title:"inspect_context"
      "no enclosing around: %a" Lexing.print_position pos;
    Some Unknown
  | enclosings ->
    let open Browse_raw in
    let node = Browse_tree.of_browse enclosings in
    log ~title:"inspect_context" "current node is: %s"
      (string_of_node node.Browse_tree.t_node);
    match node.Browse_tree.t_node with
    | Pattern p -> inspect_pattern ~pos ~lid p
    | Value_description _
    | Type_declaration _
    | Extension_constructor _
    | Module_binding_name _
    | Module_declaration_name _ ->
      None
    | Module_expr _
    | Open_description _ -> Some Module_path
    | Module_type _ -> Some Module_type
    | Core_type _ -> Some Type
    | Record_field (_, lbl, _) when (Longident.last lid) = lbl.lbl_name ->
      (* if we stopped here, then we're on the label itself, and whether or
          not punning is happening is not important *)
      Some (Label lbl)
    | Expression e -> Some (inspect_expression ~pos ~lid e)
    | _ ->
      Some Unknown
