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

  let errors t = [] (* FIXME *)
  let comments t = [] (* FIXME *)
end

type spec =
  | Normal of Extension.set * Merlin_parser.kind
  | PP of string * Merlin_parser.kind
  | External of string * string list * Merlin_parser.kind 

type t =
  | Is_normal of Merlin_parser.t
  | Is_pp of PP.t

let make spec src = match spec with
  | Normal (ext, kind) ->
    let lexer = Merlin_lexer.make (Extension.keywords ext) src in
    let parser = Merlin_parser.make lexer kind in
    Is_normal parser
  | PP (pp, kind) ->
    Is_pp (PP.make pp kind src)
  | External _ -> failwith "TODO"

let update src = function
  | Is_normal parser as t ->
    let lexer = Merlin_parser.lexer parser in
    let lexer = Merlin_lexer.update src lexer in
    let parser' = Merlin_parser.update lexer parser in
    if parser == parser' then t
    else Is_normal parser'
  | Is_pp pp ->
    Is_pp (PP.update src pp)

let result = function
  | Is_normal parser -> Merlin_parser.result parser
  | Is_pp pp -> PP.result pp

let source = function
  | Is_normal parser -> Merlin_lexer.source (Merlin_parser.lexer parser)
  | Is_pp pp -> PP.source pp

let compare a b = match a, b with
  | Is_normal _, Is_pp _ -> -1
  | Is_pp _, Is_normal _ -> 1
  | Is_normal a, Is_normal b ->
    Merlin_parser.compare a b
  | Is_pp a, Is_pp b ->
    PP.compare a b

let is_normal = function
  | Is_normal p -> Some p
  | Is_pp _ -> None

let find_lexer = function
  | Is_normal p -> Some (Merlin_parser.lexer p)
  | Is_pp _ -> None

let errors = function
  | Is_normal p ->
    Merlin_lexer.errors (Merlin_parser.lexer p) @
    Merlin_parser.errors p
  | Is_pp pp ->
    PP.errors pp

let comments = function
  | Is_normal p ->
    Merlin_lexer.comments (Merlin_parser.lexer p)
  | Is_pp pp ->
    PP.comments pp

let default_keywords = Lexer_raw.keywords []

let reconstruct_identifier ?for_locate t pos =
  (* FIXME: external should delegate identifier reconstruction to custom implementation *)
  let lexer = match t with
    | Is_normal p -> Merlin_parser.lexer p
    | Is_pp pp ->
      let source = PP.source pp in
      Merlin_lexer.make default_keywords source
  in
  Merlin_lexer.reconstruct_identifier ?for_locate lexer pos

let for_completion t pos =
  match t with
  | Is_normal p ->
    let lexer = Merlin_parser.lexer p in
    let no_labels, lexer = Merlin_lexer.for_completion lexer pos in
    no_labels, Is_normal (Merlin_parser.update lexer p)
  | Is_pp _ ->
    `No_labels false, t

let trace t nav = match t with
  | Is_normal p ->
    Merlin_parser.trace p nav
  | Is_pp _ -> ()
