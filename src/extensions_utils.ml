(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

let ident = Ident.create "_"

let parse_sig str =
  let buf = Lexing.from_string str in
  Chunk_parser.interface Lexer.token buf

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let always, registry =
  let f = List.map
    ~f:(fun {Extensions. name; private_def; public_def; keywords; packages} ->
      name,
      (List.concat_map ~f:parse_sig private_def,
       List.concat_map ~f:parse_sig public_def,
       keywords,
       packages))
  in
  Extensions.(List.map ~f:snd (f always), f registry)
let all_extensions = List.map ~f:fst registry
let ext_table = Hashtbl.create 5

let list = function
  | `All ->
    all_extensions
  | `Enabled ->
    Hashtbl.fold (fun name _ names -> name :: names) ext_table []
  | `Disabled ->
    List.filter (fun name -> not (Hashtbl.mem ext_table name)) all_extensions

let parser_valid = ref (ref false)

let set_raw_extension ~enabled (name,(_,_,kw,_ as ext)) =
  Lexer.set_extension ~enabled kw;
  !parser_valid := false;
  if enabled
  then Hashtbl.replace ext_table name ext
  else Hashtbl.remove ext_table name

let set_extension ~enabled name =
  try  let ext = List.assoc name registry in
       set_raw_extension ~enabled (name,ext)
  with Not_found -> ()

let set_extensions list =
  List.iter
    (fun ext ->
       let enabled = (List.mem ext list) in
       if enabled <> Hashtbl.mem ext_table ext then
         set_extension ~enabled ext)
    all_extensions

let extensions_from_packages packages =
  List.filter_map registry
    ~f:(fun (ext,(_,_,_,ps)) ->
        if List.exists (fun p -> List.mem p packages) ps
        then Some ext
        else None)

let parser_valid () =
  if not !(!parser_valid) then
    parser_valid := ref true;
  !parser_valid

let register env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with _exn -> [] in
  let enabled = always @ Hashtbl.fold (fun _ ext exts -> ext :: exts)
                          ext_table []
  in
  let fakes, tops =
    List.split
      (List.map (fun (fake,top,_,_) -> try_type fake, try_type top) enabled)
  in
  let add_hidden_signature env sign =
    let add_item env comp =
      match comp with
      | Types.Sig_value(id, decl)     -> Env.add_value (Ident.hide id) decl env
      | Types.Sig_type(id, decl, _)   -> Env.add_type (Ident.hide id) decl env
      | Types.Sig_exception(id, decl) -> Env.add_exception (Ident.hide id) decl env
      | Types.Sig_module(id, mty, _)  -> Env.add_module (Ident.hide id) mty env
      | Types.Sig_modtype(id, decl)   -> Env.add_modtype (Ident.hide id) decl env
      | Types.Sig_class(id, decl, _)  -> Env.add_class (Ident.hide id) decl env
      | Types.Sig_class_type(id, decl, _) -> Env.add_cltype (Ident.hide id) decl env
    in
    List.fold_left ~f:add_item ~init:env sign
  in
  let env = add_hidden_signature env (List.concat tops) in
  let env = Env.add_module ident (Types.Mty_signature (lazy (List.concat fakes))) env in
  env
