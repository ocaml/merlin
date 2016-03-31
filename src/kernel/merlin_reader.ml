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

  let loaded_driver t =
    let open Extend_main in
    let open Protocol_def.Reader in
    let section = "(ext)" ^ t.name in
    let notify str = Logger.notify section "%s" str in
    let debug str = Logger.log "reader" section str in
    let driver = Driver.run t.name in
    let reader cmd = Driver.reader ~notify ~debug driver cmd in
    let buffer = {
      Reader_def.
      path  = Merlin_source.filename t.source;
      flags = t.args;
      text  = Merlin_source.text t.source;
    } in
    match reader (Load buffer) with
    | Ret_loaded -> driver, reader
    | _ ->
      Driver.stop driver;
      failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)

  let parsetree t = function
    | Reader_def.Signature sg -> `Signature sg
    | Reader_def.Structure str -> `Structure str
    | _ -> failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)

  let result t = match t.result with
    | Some r -> r
    | None ->
      let driver, reader = loaded_driver t in
      let parsetree = match reader Protocol_def.Reader.Parse with
        | Protocol_def.Reader.Ret_tree tree -> parsetree t tree
        | _ -> failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)
      in
      Extend_main.Driver.stop driver;
      t.result <- Some parsetree;
      parsetree

  let for_completion t pos =
    let driver, reader = loaded_driver t in
    let result = match reader (Protocol_def.Reader.Parse_for_completion pos) with
      | Protocol_def.Reader.Ret_tree_for_competion (info, tree) ->
        let parsetree = parsetree t tree in
        (`No_labels (not info.Reader_def.complete_labels),
         {t with result = Some parsetree})
      | _ ->
        failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)
    in
    Extend_main.Driver.stop driver;
    result

  let reconstruct_identifier t pos =
    let driver, reader = loaded_driver t in
    let ident = match reader (Protocol_def.Reader.Get_ident_at pos) with
      | Protocol_def.Reader.Ret_ident ident -> ident
      | _ ->
        failwith (Printf.sprintf "Extension %S has incorrect behavior" t.name)
    in
    Extend_main.Driver.stop driver;
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
  let from_lexer lexer = Merlin_lexer.reconstruct_identifier lexer pos in
  match t with
  | Is_normal p -> from_lexer (Merlin_parser.lexer p)
  | Is_pp pp ->
    let source = PP.source pp in
    from_lexer (Merlin_lexer.make default_keywords source)
  | Is_extend ext ->
    Extend.reconstruct_identifier ext pos

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
