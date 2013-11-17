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

type step_state = {
  exns: exn list;
  global_exns: exn list;
  env: Env.t;
  trees: Typedtree.structure Location.loc list;
  snap: Btype.snapshot;
}

module Context = struct
  type state = step_state

  type sig_item = Types.signature Location.loc list or_exn
  type str_item = Typedtree.structure Location.loc list or_exn
  type sig_in_sig_modtype = unit
  type sig_in_sig_module  = unit
  type sig_in_str_modtype = unit
  type str_in_module      = unit
end

let initial_env =
  let env = lazy begin
    Ident.reinit();
    try
      if !Clflags.nopervasives
      then Env.initial
      else Env.open_pers_signature "Pervasives" Env.initial
    with Not_found ->
      failwith "cannot open pervasives.cmi"
  end in
  fun () -> let lazy env = env in
            Extensions_utils.register env

let protect_typer f =
  let errors, result =
    Either.join (Merlin_parsing.catch_warnings (fun () -> Merlin_types.catch_errors f))
  in
  errors,
  Either.elim raise (fun x -> x) result

module Fold = struct
  (* Initial state *)
  let initial () = 
    let env = initial_env () in
    let snap = Btype.snapshot () in
    { exns = []; global_exns = []; trees = []; env; snap }

  let sig_root _ = initial ()
  let str_root _ = initial ()

  (* Fold items *)
  let sig_item _ = failwith "TODO"

  let str_item step ?back_from state =
    match Chunk.Spine.value step with
    | Either.L exn -> {state with snap = Btype.snapshot ()}, Either.L exn
    | Either.R items ->
      let exns', (env, exns, trees) =
        protect_typer
        begin fun () ->
          List.fold_left items ~init:(state.env, [], [])
          ~f:begin fun (env, exns, trees) d ->
          try
            let t,_,env =
              Typemod.type_structure env [d.Location.txt] d.Location.loc
            in
            (env, exns, {d with Location.txt = t} :: trees)
          with exn -> (env, exn :: exns, trees)
          end
        end
      in
      let exns = match back_from with
        | Some {exns = exns'} -> exns' @ exns
        | _ -> exns' @ exns
      in
      let snap = Btype.snapshot () in
      {state with exns = exns @ state.exns;
                  trees = trees @ state.trees;
                  snap; env},
      Either.R (List.rev trees)

  (* Fold structure shape *)
  let str_in_module step state =
    match Chunk.Spine.value step with
    | Either.L _exn -> {state with snap = Btype.snapshot ()}, ()
    | Either.R (_, {Location. txt = pmod; _}) ->
    match
      protect_typer
      begin fun () -> try
        let open Typedtree in
        let open Parsetree in
        let rec filter_constraint md =
          let update f = function
            | None -> None
            | Some md' -> Some (f md')
          in
          match md.pmod_desc with
            | Pmod_structure _ -> Some md
            | Pmod_functor (a,b,md) ->
                update
                  (fun md' -> {md with pmod_desc = Pmod_functor (a,b,md')})
                  (filter_constraint md)
            | Pmod_constraint (md,_) ->
                update (fun x -> x) (filter_constraint md)
            | _ -> None
        in
        let pmod = match filter_constraint pmod with
          | Some pmod' -> pmod'
          | None -> pmod
        in
        let rec find_structure md =
          match md.mod_desc with
            | Tmod_structure _ -> Some md
            | Tmod_functor (_,_,_,md) -> find_structure md
            | Tmod_constraint (md,_,_,_) -> Some md
            | _ -> None
        in
        let tymod = Typemod.type_module state.env pmod in
        match find_structure tymod with
          | None -> None
          | Some md -> Some ([], md.mod_env)
        with exn -> Some ([exn], state.env)
      end
    with
    | exns, None ->
      let snap = Btype.snapshot () in
      {state with exns; snap;
                  global_exns = state.exns @ state.global_exns}, ()
    | exns', Some (exns, env) ->
      let snap = Btype.snapshot () in
      {state with exns = exns @ exns'; snap; env;
                  global_exns = state.exns @ state.global_exns}, ()

  (* Fold signature shape *)
  let sig_in_sig_modtype _ = failwith "TODO"
  let sig_in_sig_module  _ = failwith "TODO"
  let sig_in_str_modtype _ = failwith "TODO"
end

module Spine = Spine.Transform (Context) (Chunk.Spine) (Fold)
type t = Spine.t
let update = Spine.update

let exns  t = let s = Spine.get_state t in s.exns @ s.global_exns
let env   t = (Spine.get_state t).env
let trees t = (Spine.get_state t).trees
let snapshot t = (Spine.get_state t).snap
