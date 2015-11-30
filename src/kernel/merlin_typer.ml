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

type tree = [
  | `Signature of Typedtree.signature
  | `Structure of Typedtree.structure
]

type data = {
  env: Env.t;
  result: tree;
  delayed_checks: Typecore.delayed_check list;
  errors: exn list;
}

type t = {
  parser: Merlin_parser.t;
  data: data lazy_t;
  extensions: String.Set.t;
  btype_cache: Btype.cache;
  env_cache: Env.cache;
}

let is_valid _ = true

let with_typer btype' env' f =
  let open Fluid in
  let' (from_ref Btype.cache) btype' @@ fun () ->
  let' (from_ref Env.cache)   env' f

let data extensions btype_cache env_cache parser = lazy begin
  let caught = ref [] in
  with_typer btype_cache env_cache @@ fun () ->
  Parsing_aux.catch_warnings caught @@ fun () ->
  Typing_aux.catch_errors caught    @@ fun () ->
  let env = Raw_typer.fresh_env () in
  let env = Env.open_pers_signature "Pervasives" env in
  let env = Extension.register extensions env in
  Typecore.delayed_checks := [];
  let result, env =
    match Merlin_parser.result parser with
    | `Signature sg ->
      let sg = Typemod.transl_signature env sg in
      `Signature sg, sg.Typedtree.sig_final_env
    | `Structure str ->
      let str, _, env = Typemod.type_structure env str Location.none in
      `Structure str, env
  in {
    env; result; errors = !caught;
    delayed_checks = !Typecore.delayed_checks;
  }
end

let make parser extensions =
  let btype_cache = Btype.new_cache () in
  let env_cache = Env.new_cache ~unit_name:"cul" in
  { parser; extensions; btype_cache; env_cache;
    data = data extensions btype_cache env_cache parser; }

let update parser t =
  if not (is_valid t) then
    make parser t.extensions
  else if t.parser == parser then
    t
  else if Merlin_parser.compare parser t.parser = 0 then
    {t with parser}
  else
    {t with parser; data = data t.extensions t.btype_cache t.env_cache parser}

let data t = Lazy.force t.data

(* Public API *)

let with_typer t f =
  with_typer t.btype_cache t.env_cache f

let result t = (data t).result
let errors t = (data t).errors

let checks t =
  Typecore.delayed_checks := (data t).delayed_checks;
  let caught = ref [] in
  Parsing_aux.catch_warnings caught (fun () ->
      Typing_aux.catch_errors caught Typecore.force_delayed_checks);
  !caught

let env t = (data t).env

let extensions t = t.extensions

let to_browse = function
  | `Signature s -> Merlin_browse.of_signature s
  | `Structure s -> Merlin_browse.of_structure s

let node_at ?(skip_recovered=false) typer pos_cursor =
  let structures = to_browse (result typer) in
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | List.More ((_,node), (List.More ((_,node'), _) as ancestors))
      when Merlin_browse.is_recovered node' -> select ancestors
    | l -> l
  in
  match Merlin_browse.deepest_before pos_cursor [structures] with
  | Some path when skip_recovered -> select path
  | Some path -> path
  | None -> List.One (env typer, Browse_node.Dummy)

let parser t = t.parser
