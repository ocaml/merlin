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

module R = Extend_protocol.Reader

module PP = struct
  type t = {
    command: string;
    kind: Merlin_parser.kind;
    source: Merlin_source.t;
    mutable result: Merlin_parser.tree option;
  }

  let make command kind source =
    { command; kind; source; result = None }

  let update source t =
    if source == t.source then
      t
    else if Merlin_source.compare source t.source = 0 then
      {t with source}
    else
      {t with source; result = None }

  let compare = compare

  let result t =
    match t.result with
    | Some r -> r
    | None ->
      Logger.log "merlin_reader" "pp filename"
        (Merlin_source.filename t.source);
      Pparse.apply_pp
        ~filename:(Merlin_source.filename t.source)
        ~source:(Merlin_source.text t.source)
        ~pp:t.command

  let source t = t.source
end

module Extend = struct
  open Extend_protocol.Reader

  type t = {
    name: string;
    args: string list;
    kind: Merlin_parser.kind;
    source: Merlin_source.t;
    mutable result: Merlin_parser.tree option;
  }

  let make name args kind source =
    { kind; source; result = None; name; args }

  let update source t =
    if source == t.source then
      t
    else if Merlin_source.compare source t.source = 0 then
      {t with source}
    else
      {t with source; result = None }

  let compare = compare

  let source t = t.source

  let start_process name =
    let section = "(ext)" ^ name in
    let notify str = Logger.notify section "%s" str in
    let debug str = Logger.log "reader" section str in
    Extend_driver.run ~notify ~debug name

  let loaded_driver t =
    let driver = start_process t.name in
    let buffer = {
      path  = Merlin_source.filename t.source;
      flags = t.args;
      text  = Merlin_source.text t.source;
    } in
    match Extend_driver.reader driver (Req_load buffer) with
    | Res_loaded -> driver
    | _ ->
      Extend_driver.stop driver;
      failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)

  let parsetree t = function
    | Signature sg -> `Signature sg
    | Structure str -> `Structure str

  let result t = match t.result with
    | Some r -> r
    | None ->
      let driver = loaded_driver t in
      let parsetree = match Extend_driver.reader driver Req_parse with
        | Res_parse ast -> parsetree t ast
        | _ -> failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)
      in
      Extend_driver.stop driver;
      t.result <- Some parsetree;
      parsetree

  let for_completion t pos =
    let driver = loaded_driver t in
    let result =
      match Extend_driver.reader driver (Req_parse_for_completion pos) with
      | Res_parse_for_completion (info, ast) ->
        let parsetree = parsetree t ast in
        (`No_labels (not info.complete_labels),
         {t with result = Some parsetree})
      | _ ->
        failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)
    in
    Extend_driver.stop driver;
    result

  let reconstruct_identifier t pos =
    let driver = loaded_driver t in
    let ident =
      match Extend_driver.reader driver (Req_get_ident_at pos) with
      | Res_get_ident_at ident -> ident
      | _ ->
        failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)
    in
    Extend_driver.stop driver;
    ident

  let source t = t.source
end

type spec =
  | Normal of Extension.set * Merlin_parser.kind
  | PP of string * Merlin_parser.kind
  | External of string * string list * Merlin_parser.kind

type t =
  | Is_normal of Merlin_parser.t
  | Is_pp of PP.t
  | Is_extend of Extend.t

let make spec src = match spec with
  | Normal (ext, kind) ->
    let lexer = Merlin_lexer.make (Extension.keywords ext) src in
    let parser = Merlin_parser.make lexer kind in
    Is_normal parser
  | PP (pp, kind) ->
    Is_pp (PP.make pp kind src)
  | External (name,args,kind) ->
    Is_extend (Extend.make name args kind src)

let update src = function
  | Is_normal parser as t ->
    let lexer = Merlin_parser.lexer parser in
    let lexer = Merlin_lexer.update src lexer in
    let parser' = Merlin_parser.update lexer parser in
    if parser == parser' then t
    else Is_normal parser'
  | Is_pp pp ->
    Is_pp (PP.update src pp)
  | Is_extend ext ->
    Is_extend (Extend.update src ext)

let result = function
  | Is_normal parser -> Merlin_parser.result parser
  | Is_pp pp -> PP.result pp
  | Is_extend ext -> Extend.result ext

let source = function
  | Is_normal parser -> Merlin_lexer.source (Merlin_parser.lexer parser)
  | Is_pp pp -> PP.source pp
  | Is_extend ext -> Extend.source ext

let compare a b = match a, b with
  | Is_normal a, Is_normal b ->
    Merlin_parser.compare a b
  | Is_pp a, Is_pp b ->
    PP.compare a b
  | Is_extend a, Is_extend b ->
    Extend.compare a b
  | Is_normal _, _ -> -1
  | _, Is_normal _ -> 1
  | Is_pp _, _ -> -1
  | _, Is_pp _ -> 1

let is_normal = function
  | Is_normal p -> Some p
  | Is_pp _ | Is_extend _ -> None

let find_lexer = function
  | Is_normal p -> Some (Merlin_parser.lexer p)
  | Is_pp _ | Is_extend _ -> None

let errors = function
  | Is_normal p ->
    Merlin_lexer.errors (Merlin_parser.lexer p) @
    Merlin_parser.errors p
  | Is_pp _ | Is_extend _ -> []

let comments = function
  | Is_normal p ->
    Merlin_lexer.comments (Merlin_parser.lexer p)
  | Is_pp _ | Is_extend _ -> []

let default_keywords = Lexer_raw.keywords []

let reconstruct_identifier t pos =
  match t with
  | Is_normal p ->
    Merlin_lexer.reconstruct_identifier
      (Merlin_lexer.source (Merlin_parser.lexer p)) pos
  | Is_pp pp ->
    Merlin_lexer.reconstruct_identifier
      (PP.source pp) pos
  | Is_extend ext ->
    let result = Extend.reconstruct_identifier ext pos in
    if result = [] then
      Merlin_lexer.reconstruct_identifier
        (Extend.source ext) pos
    else
      result

let for_completion t pos =
  match t with
  | Is_normal p ->
    let lexer = Merlin_parser.lexer p in
    let no_labels, lexer = Merlin_lexer.for_completion lexer pos in
    no_labels, Is_normal (Merlin_parser.update lexer p)
  | Is_pp _ ->
    `No_labels false, t
  | Is_extend ext ->
    let no_labels, ext = Extend.for_completion ext pos in
    no_labels, Is_extend ext

let trace t nav = match t with
  | Is_normal p ->
    Merlin_parser.trace p nav
  | Is_pp _ | Is_extend _ -> ()

module With_reader = struct

  open Extend_protocol.Reader

  let reader = ref None

  module Oprint = struct
    let default_out_value          = !Oprint.out_value
    let default_out_type           = !Oprint.out_type
    let default_out_class_type     = !Oprint.out_class_type
    let default_out_module_type    = !Oprint.out_module_type
    let default_out_sig_item       = !Oprint.out_sig_item
    let default_out_signature      = !Oprint.out_signature
    let default_out_type_extension = !Oprint.out_type_extension
    let default_out_phrase         = !Oprint.out_phrase

    let oprint default inj ppf x = match !reader with
      | None -> default ppf x
      | Some (name, driver) ->
        begin match Extend_driver.reader driver (Req_print_outcome [inj x]) with
          | Res_print_outcome [x] ->
            Format.pp_print_string ppf x
          | _ ->
            failwith (Printf.sprintf "Extension %S has incorrect behavior" name)
        end

    let () =
      Oprint.out_value :=
        oprint default_out_value (fun x -> Out_value x);
      Oprint.out_type :=
        oprint default_out_type (fun x -> Out_type x);
      Oprint.out_class_type :=
        oprint default_out_class_type (fun x -> Out_class_type x);
      Oprint.out_module_type :=
        oprint default_out_module_type (fun x -> Out_module_type x);
      Oprint.out_sig_item :=
        oprint default_out_sig_item (fun x -> Out_sig_item x);
      Oprint.out_signature :=
        oprint default_out_signature (fun x -> Out_signature x);
      Oprint.out_type_extension :=
        oprint default_out_type_extension (fun x -> Out_type_extension x);
      Oprint.out_phrase :=
        oprint default_out_phrase (fun x -> Out_phrase x)
  end

  let with_reader t f = match t with
    | Is_normal _ | Is_pp _ -> f ()
    | Is_extend ext ->
      let driver = Extend.loaded_driver ext in
      reader := Some (ext.Extend.name, driver);
      Misc.try_finally f
        (fun () ->
           reader := None;
           Extend_driver.stop driver)

  let attr_cleaner =
    {Ast_mapper.default_mapper with
     Ast_mapper.attributes =
       (fun _ attrs ->
          List.filter (fun (name,_) ->
              not (Std.String.is_prefixed ~by:"merlin." name.Location.txt))
            attrs);
     Ast_mapper.extension =
       (fun _ ext ->
          match ext with
          | name, Parsetree.PCustom _ -> name, Parsetree.PStr []
          | ext -> ext)
    }

  let pprint t =
    let t = match t with
      | Pretty_case_list x ->
        Pretty_case_list (attr_cleaner.Ast_mapper.cases attr_cleaner x)
      | Pretty_core_type x ->
        Pretty_core_type (attr_cleaner.Ast_mapper.typ attr_cleaner x)
      | Pretty_expression x ->
        Pretty_expression (attr_cleaner.Ast_mapper.expr attr_cleaner x)
      | Pretty_pattern x ->
        Pretty_pattern (attr_cleaner.Ast_mapper.pat attr_cleaner x)
      | Pretty_signature x ->
        Pretty_signature (attr_cleaner.Ast_mapper.signature attr_cleaner x)
      | Pretty_structure x ->
        Pretty_structure (attr_cleaner.Ast_mapper.structure attr_cleaner x)
      | Pretty_toplevel_phrase (Parsetree.Ptop_def x) ->
        let x = attr_cleaner.Ast_mapper.structure attr_cleaner x in
        Pretty_toplevel_phrase (Parsetree.Ptop_def x)
      | Pretty_toplevel_phrase (Parsetree.Ptop_dir _) as t -> t
    in
    let parse_tree_to_string t =
      let ppf, to_string = Std.Format.to_string () in
      begin match t with
        | Pretty_case_list       x -> Pprintast.case_list       ppf x
        | Pretty_core_type       x -> Pprintast.core_type       ppf x
        | Pretty_expression      x -> Pprintast.expression      ppf x
        | Pretty_pattern         x -> Pprintast.pattern         ppf x
        | Pretty_signature       x -> Pprintast.signature       ppf x
        | Pretty_structure       x -> Pprintast.structure       ppf x
        | Pretty_toplevel_phrase x -> Pprintast.toplevel_phrase ppf x
      end;
      to_string ()
    in
    match !reader with
    | None -> parse_tree_to_string t
    | Some (name, driver)->
      begin match Extend_driver.reader driver (Req_pretty_print t) with
        | Res_pretty_print str -> str
        | _ -> failwith (Printf.sprintf "Extension %S has incorrect behavior" name)
      end


  let oprint_list ts = match !reader with
    | None ->
      let print t =
        Extend_helper.print_outcome_using_oprint Format.str_formatter t;
        Format.flush_str_formatter ()
      in
      List.rev (List.rev_map print ts)
    | Some (name, driver)->
      match Extend_driver.reader driver
              (Extend_protocol.Reader.Req_print_outcome ts) with
      | Extend_protocol.Reader.Res_print_outcome ts -> ts
      | _ -> failwith (Printf.sprintf "Extension %S has incorrect behavior" name)
end

let with_reader =
  With_reader.with_reader
let oprint_list =
  With_reader.oprint_list
let pprint =
  With_reader.pprint

let has_extend_support name =
  try
    let driver = Extend.start_process name in
    Extend_driver.stop driver;
    true
  with _ -> false
