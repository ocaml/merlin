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
open Misc

open Protocol
open Merlin_lib

type state = {
  mutable project : Project.t;
  mutable buffer : Buffer.t;
  mutable lexer : Lexer.t option;
}

let new_state () =
  let project = Project.create () in
  let buffer = Buffer.create project Parser.implementation in
  { project; buffer; lexer = None }

let dispatch (i,o : IO.io) (state : state) =
  fun (type a) (request : a request) ->
  (match request with
  | (Tell source : a request) ->
    let lexer = match state.lexer with
      | Some lexer when not (Lexer.eof lexer) -> lexer
      | None | Some _ ->
      let lexer = Buffer.start_lexing state.buffer in
      state.lexer <- Some lexer; lexer
    in
    assert (Lexer.feed lexer source);
    ignore (Buffer.update state.buffer (Lexer.history lexer));
    Lexer.position lexer, Buffer.path state.buffer

  | (Type_expr (source, None) : a request) ->
    failwith "TODO"

  | (Type_expr (source, Some pos) : a request) ->
    failwith "TODO"

  | (Type_enclosing ((expr, offset), pos) : a request) ->
    failwith "TODO"

  | (Complete_prefix (prefix, None) : a request) ->
    failwith "TODO"

  | (Complete_prefix (prefix, Some pos) : a request) ->
    failwith "TODO"

  | (Locate (path, opt_pos) : a request) ->
    failwith "TODO"

  | (Drop : a request) ->
    failwith "TODO"

  | (Seek `Position : a request) ->
    failwith "TODO"

  | (Seek (`Before pos) : a request) ->
    failwith "TODO"

  | (Seek (`Exact pos) : a request) ->
    failwith "TODO"

  | (Seek `End : a request) ->
    failwith "TODO"

  | (Boundary (dir,pos) : a request) ->
    failwith "TODO"

  | (Reset (ml,source) : a request) ->
    failwith "TODO"

  | (Refresh : a request) ->
    failwith "TODO"

  | (Errors : a request) ->
    failwith "TODO"

  | (Dump _ : a request) ->
    failwith "TODO"

  | (Which_path s : a request) ->
    failwith "TODO"

  | (Which_with_ext ext : a request) ->
    failwith "TODO"

  | (Project_load (cmd,path) : a request) ->
    failwith "TODO"

  | (Findlib_list : a request) ->
    Fl_package_base.list_packages ()

  | (Findlib_use packages : a request) ->
    failwith "TODO"

  | (Extension_list kind : a request) ->
    []

  | (Extension_set (action,extensions) : a request) ->
    failwith "TODO"

  | (Path (var,action,pathes) : a request) ->
    failwith "TODO"

  | (Path_list `Build : a request) ->
    failwith "TODO"

  | (Path_list `Source : a request) ->
    failwith "TODO"

  | (Path_reset : a request) ->
    failwith "TODO"

  : a)
