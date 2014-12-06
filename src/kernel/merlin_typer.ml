(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

let caught catch =
  let caught = !catch in
  catch := [];
  caught

let rec last_env t =
  let rec last candidate = function
    | [] -> candidate
    | x :: xs ->
      last (if Lexing.compare_pos
               x.BrowseT.t_loc.Location.loc_start
               candidate.BrowseT.t_loc.Location.loc_start
               > 0
            then x
            else candidate)
        xs
  in
  let t' = last t (Lazy.force t.BrowseT.t_children) in
  if t == t' then
    t'
  else
    last_env t'

module P = struct

  open Raw_parser

  type st = Extension.set * exn list ref

  type t = {
    raw        : Raw_typer.t;
    snapshot   : Btype.snapshot;
    env        : Env.t;
    contents   : [`Str of Typedtree.structure | `Sg of Typedtree.signature] list;
    exns       : exn list;
  }

  let empty (extensions,catch) =
    let env = Raw_typer.fresh_env () in
    let env = Env.open_pers_signature "Pervasives" env in
    let env = Extension.register extensions env in
    let raw = Raw_typer.empty in
    let exns = caught catch in
    let snapshot = Btype.snapshot () in
    { raw; snapshot; env; contents = []; exns }

  let validate _ t = Btype.is_valid t.snapshot

  let append catch loc item t =
    try
      Btype.backtrack t.snapshot;
      let env, contents =
        match item with
        | `str str ->
          let structure,_,env = Typemod.type_structure t.env str loc in
          env, `Str structure :: t.contents
        | `sg sg ->
          let sg = Typemod.transl_signature t.env sg in
          sg.Typedtree.sig_final_env, `Sg sg :: t.contents
        | `fake str ->
          let structure,_,_ =
            Parsing_aux.catch_warnings (ref [])
              (fun () -> Typemod.type_structure t.env [str] loc)
          in
          let browse =
            BrowseT.of_node ~loc ~env:t.env (BrowseT.Structure structure)
          in
          (last_env browse).BrowseT.t_env, `Str structure :: t.contents
        | `none -> t.env, t.contents
      in
      Typecore.reset_delayed_checks ();
      {env; contents; snapshot = Btype.snapshot (); raw = t.raw;
       exns = caught catch @ t.exns}
    with exn ->
      Typecore.reset_delayed_checks ();
      let snapshot = Btype.snapshot () in
      {t with snapshot; exns = exn :: caught catch @ t.exns}

  let rewrite loc = function
    | Raw_typer.Functor_argument (id,mty) ->
      let mexpr = Ast_helper.Mod.structure ~loc [] in
      let mexpr = Ast_helper.Mod.functor_ ~loc id mty mexpr in
      let mb = Ast_helper.Mb.mk (Location.mknoloc "") mexpr in
      `fake (Ast_helper.Str.module_ ~loc mb)
    | Raw_typer.Pattern (l,o,p) ->
      let expr = Ast_helper.Exp.constant ~loc (Asttypes.Const_int 0) in
      let expr = Ast_helper.Exp.fun_ ~loc l o p expr in
      `fake (Ast_helper.Str.eval ~loc expr)
    | Raw_typer.Newtype s ->
      let expr = Ast_helper.Exp.constant (Asttypes.Const_int 0) in
      let patt = Ast_helper.Pat.any () in
      let expr = Ast_helper.Exp.fun_ "" None patt expr in
      let expr = Ast_helper.Exp.newtype ~loc s expr in
      `fake (Ast_helper.Str.eval ~loc expr)
    | Raw_typer.Bindings (rec_,e) ->
      `str [Ast_helper.Str.value ~loc rec_ e]
    | Raw_typer.Open (override,name) ->
      let od = Ast_helper.Opn.mk ~override name in
      `str [Ast_helper.Str.open_ ~loc od]
    | Raw_typer.Eval e ->
      `str [Ast_helper.Str.eval ~loc e]
    | Raw_typer.Structure str ->
      `str str
    | Raw_typer.Signature sg ->
      `sg sg

  let frame (_,catch) f t =
    let module Frame = Merlin_parser.Frame in
    let loc = Frame.location f in
    let raw = Raw_typer.step (Frame.value f) t.raw in
    let t = {t with raw} in
    let items = Raw_typer.observe t.raw in
    let items = List.map ~f:(rewrite loc) items in
    let t = List.fold_left' ~f:(append catch loc) items ~init:t in
    t
end

module I = struct
  type t =
    | Zero of P.t
    | More of P.t * Merlin_parser.frame * t

  let empty st = Zero (P.empty st)

  let previous = function
    | Zero _ -> None
    | More (_,_,t) -> Some t

  let value (Zero x | More (x,_,_)) = x

  let update st f t =
    let t, frames = match t with
      | Zero _ -> t, None
      | More (_,f',_) ->
        try
          let f0 = Merlin_parser.root_frame f f' in
          let rec mount = function
            | Zero _ as t -> t, None
            | More (p,f',_) as t
              when Merlin_parser.Frame.eq f0 f' &&
                   P.validate st p ->
              t, Some (Merlin_parser.unroll_stack ~from:f ~root:f')
            | More (_,_,t) -> mount t
          in
          mount t
        with Not_found ->
          empty st, None
    in
    let frames = match frames with
      | Some frames -> frames
      | None -> List.rev (f :: List.unfold Merlin_parser.Frame.next f)
    in
    let process_frame f t = More (P.frame st f (value t), f, t) in
    List.fold_left' ~f:process_frame frames ~init:t

  let update' st p t = update st (Merlin_parser.stack p) t
end

type t = {
  btype_cache : Btype.cache;
  env_cache   : Env.cache;
  extensions  : Extension.set;
  typer       : I.t;
  stamp       : bool ref list;
}

let fluid_btype = Fluid.from_ref Btype.cache
let fluid_env = Fluid.from_ref Env.cache

let protect_typer ~btype ~env f =
  let caught = ref [] in
  let (>>=) f x = f x in
  Fluid.let' fluid_btype btype >>= fun () ->
  Fluid.let' fluid_env env >>= fun () ->
  Either.join (Parsing_aux.catch_warnings caught >>= fun () ->
               Typing_aux.catch_errors caught >>= fun () ->
               f caught)

let fresh ~unit_name ~stamp extensions =
  let btype_cache = Btype.new_cache () in
  let env_cache = Env.new_cache ~unit_name in
  let result = protect_typer ~btype:btype_cache ~env:env_cache (fun exns ->
    Either.try' (fun () -> I.empty (extensions,exns))
  )
  in
  {
    stamp;
    typer = Either.get result;
    extensions; env_cache; btype_cache;
  }

let get_incremental _state x = x
let get_value = I.value

let update parser t =
  let result =
    protect_typer ~btype:t.btype_cache ~env:t.env_cache (fun exns ->
    Either.try' (fun () ->
      let state = (t.extensions,exns) in
      I.update' state parser (get_incremental state t.typer)
    )
  )
  in
  {t with typer = Either.get result}

let env t = (get_value t.typer).P.env
let contents t = (get_value t.typer).P.contents
let exns t = (get_value t.typer).P.exns
let extensions t = t.extensions

let is_valid t =
  List.for_all ~f:(!) t.stamp &&
  match
    protect_typer ~btype:t.btype_cache ~env:t.env_cache
      (fun _ -> Either.try' Env.check_cache_consistency)
  with
  | Either.L _exn -> false
  | Either.R result -> result

let dump ppf t =
  let ls = t.typer :: List.unfold I.previous t.typer in
  let ts = List.map ~f:I.value ls in
  let ts = List.map ts ~f:(fun x -> x.P.raw) in
  List.iter (Raw_typer.dump ppf) ts

let with_typer t f =
  Fluid.let' fluid_btype t.btype_cache (fun () ->
  Fluid.let' fluid_env t.env_cache (fun () ->
  f t))
