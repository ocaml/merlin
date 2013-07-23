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

type state = Env.t * (Typedtree.structure * Types.signature) list * exn list
type item = Chunk.sync * state
type sync = item History.sync
type t = item History.t

let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    failwith "cannot open pervasives.cmi"

let initial_env =
  let cenv = Lazy.lazy_from_fun initial_env in
  fun () ->
    let env = Lazy.force cenv in
    Extensions_utils.register env

let value t =
  match History.prev t with
    | None -> (initial_env (), [], [])
    | Some (_,item) -> item

let env   t = let v,_,_ = value t in v
let trees t = let _,v,_ = value t in v
let exns  t = let _,_,v = value t in v

let append_step chunks chunk_item t =
  let env, trees, exns = value t in
  match chunk_item with
    | Chunk.Module_opening (_,_,pmod) ->
        begin try
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
          let tymod = Typemod.type_module env pmod in
          match find_structure tymod with
            | None -> None
            | Some md -> Some (md.mod_env, trees, exns)
        with exn -> Some (env, trees, exn :: exns)
        end

    | Chunk.Definitions ds ->
        let (env,trees,exns) =
          List.fold_left
          begin fun (env,trees,exns) d ->
          try
            let tstr,tsg,env = Typemod.type_structure env [d.Location.txt] d.Location.loc in
            (env, (tstr,tsg) :: trees, exns)
          with exn -> (env, trees, exn :: exns)
          end (env,trees,exns) ds
        in
        Some (env, trees, exns)

    | Chunk.Module_closing (d,offset) ->
        begin try
          let _, t = History.Sync.rewind fst (History.seek_offset offset chunks) t in
          let env, trees, exns = value t in
          let tstr,tsg,env = Typemod.type_structure env [d.Location.txt] d.Location.loc in
          Some (env, (tstr,tsg) :: trees, exns)
        with exn -> Some (env, trees, exn :: exns)
        end

let sync chunks t =
  (* Find last synchronisation point *)
  let chunks, t = History.Sync.rewind fst chunks t in
  (* Drop out of sync items *)
  let t = History.cutoff t in
  (* Process last items *)
  let rec aux chunks t =
    match History.forward chunks with
      | None -> t
      | Some ((_,(_,chunk)),chunks') ->
          let type_errs, (env, trees, exns') =
            let type_errs, item = match chunk with
              | Misc.Inr c ->
                  let errs, result =
                    let process () = append_step chunks c t in
                    let errors ()  = Merlin_types.catch_errors process in
                    Misc.catch_join (Merlin_parsing.catch_warnings errors)
                  in
                  errs, Misc.sum raise (fun x -> x) result
              | Misc.Inl _ -> [], None
            in
            type_errs,
            match item with
              | Some result -> result
              | None -> value t
          in
          aux chunks'
            (History.(insert (Sync.at chunks', (env, trees, type_errs @ exns'))) t)
  in
  aux chunks t
