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
open Browse_node

module Parser = Merlin_parser

(* All exceptions are added to an [exn list ref] referred to as "caught" in the
   rest of the file.
   This helper just extracs exceptions and reset the list. *)

let mk_arg_lbl = Raw_compat.Parsetree.arg_label_of_str

let caught catch =
  let caught = !catch in
  catch := [];
  caught

type parsed = [ `Str of Parsetree.structure | `Sg of Parsetree.signature ]
type typed  = [ `Str of Typedtree.structure | `Sg of Typedtree.signature
              | `Fail of Env.t * Location.t ]
type checks = Typecore.delayed_check list

(* Intermediate, resumable type checking state *)

type step = {
  raw        : Raw_typer.t;
  ppx_cookie : Ast_mapper.cache;
  snapshot   : Btype.snapshot;
  env        : Env.t;
  contents   : (parsed * typed * checks) list;
  exns       : exn list;
}

let empty extensions catch  =
  let env = Raw_typer.fresh_env () in
  let env = Env.open_pers_signature "Pervasives" env in
  let env = Extension.register extensions env in
  let exns = caught catch in
  let snapshot = Btype.snapshot () in
  let ppx_cookie = !Ast_mapper.cache in
  { raw = Raw_typer.empty;
    contents = [];
    ppx_cookie; snapshot; env; exns;
  }

(* Rewriting:
   - internal, to turn partial ASTs into valid OCaml AST chunks
   - external by applying ppx preprocessors
*)
let fake_loc =
  let pos = {Lexing.dummy_pos with Lexing.pos_fname = "fake"} in
  let open Location in
  {loc_ghost = true; loc_start = pos; loc_end = pos}

let fake_expr = Ast_helper.Exp.constant ~loc:fake_loc (Asttypes.Const_int 0)
let fake_str = Ast_helper.Mod.structure ~loc:fake_loc []

let rewrite_raw loc = function
  | Raw_typer.Functor_argument (id,mty) ->
    let mexpr = Ast_helper.Mod.functor_ ~loc id mty fake_str in
    let mb = Ast_helper.Mb.mk (Location.mknoloc "") mexpr in
    `fake [Ast_helper.Str.module_ ~loc mb]
  | Raw_typer.Pattern (l,o,p) ->
    let expr = Ast_helper.Exp.fun_ ~loc l o p fake_expr in
    `fake [Ast_helper.Str.eval ~loc expr]
  | Raw_typer.Newtype s ->
    let patt = Ast_helper.Pat.any () in
    let expr = Ast_helper.Exp.fun_ (mk_arg_lbl "") None patt fake_expr in
    let expr = Ast_helper.Exp.newtype ~loc s expr in
    `fake [Ast_helper.Str.eval ~loc expr]
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

let rewrite_ppx = function
  | `str str  -> `str  (Pparse.apply_rewriters_str ~tool_name:"merlin" str)
  | `sg sg    -> `sg   (Pparse.apply_rewriters_sig ~tool_name:"merlin" sg)
  | `fake str -> `fake (Pparse.apply_rewriters_str ~tool_name:"merlin" str)

let rewrite loc raw = Raw_typer.rewrite_loc (rewrite_ppx (rewrite_raw loc raw))

(* Produce a new step by processing one frame from the parser *)

let fake_env node =
  let rec select env node acc =
    Browse_node.fold_node select env node @@
    if Browse_node.node_real_loc Location.none node == fake_loc then
      env
    else
      acc
  in
  let env = Browse_node.node_update_env Env.empty node in
  select env node env

let append catch loc step item =
  try
    Typecore.delayed_checks := [];
    Fluid.set Typing_aux.relax_typer false;
    let env, contents =
      match item with
      | `str str ->
        let str',t,env = Typemod.type_structure step.env str loc in
        env,
        (`Str str, `Str str', !Typecore.delayed_checks) :: step.contents
      | `sg sg ->
        let sg' = Typemod.transl_signature step.env sg in
        let env = sg'.Typedtree.sig_final_env in
        env,
        (`Sg sg, `Sg sg', !Typecore.delayed_checks) :: step.contents
      | `fake str ->
        let str',_,_ =
          Parsing_aux.catch_warnings (ref [])
            (fun () -> Typemod.type_structure step.env str loc)
        in
        let env = fake_env (Browse_node.Structure str') in
        env,
        (`Str str, `Str str', !Typecore.delayed_checks) :: step.contents
      | `none -> step.env, step.contents
    in
    let snapshot = Btype.snapshot () in
    let ppx_cookie = !Ast_mapper.cache in
    {env; contents; snapshot; ppx_cookie;
     raw = step.raw;
     exns = caught catch @ step.exns}
  with exn ->
    let snapshot = Btype.snapshot () in
    let failed = `Fail (step.env, loc) in
    let content = match item with
      | `str str | `fake str -> Some (`Str str, failed, [])
      | `sg sg -> Some (`Sg sg, failed, [])
      | `none -> None
    in
    {step with snapshot;
               exns = exn :: caught catch @ step.exns;
               contents = Option.cons content step.contents}
(* Incremental synchronization *)

type sync_step = (Parser.frame * step)

let sync_frame catch frame (_,step) =
  let loc   = Parser.Frame.location frame in
  let value = Parser.Frame.value frame in
  let raw   = Raw_typer.step value step.raw in
  let items = Raw_typer.observe raw in
  let items = List.map ~f:(rewrite loc) items in
  let step  = List.fold_left ~f:(append catch loc) items ~init:{step with raw} in
  Typecore.delayed_checks := [];
  (frame, step)

let new_stack extensions catch frame =
  match List.rev_unfold [frame] ~f:Parser.Frame.next frame with
  | [] -> []
  | first :: rest ->
    let step = empty extensions catch in
    let init = (first, step) in
    Btype.backtrack step.snapshot;
    List.rev_scan_left [init] ~f:(sync_frame catch) rest ~init

let sync_stack extensions catch steps current_frame =
  let rec find_valid = function
    | [] -> raise Not_found
    | ((_,step as last) :: _) as steps when Btype.is_valid step.snapshot ->
      last, steps
    | _ :: rest -> find_valid rest
  in
  let rec find_root root = function
    | [] -> raise Not_found
    | ((frame',_) :: _) as steps when Parser.Frame.eq frame' root ->
      steps
    | _ :: rest -> find_root root rest
  in
  try
    match steps with
    | [] -> raise Not_found
    | (frame',_) :: _ ->
      let root = Parser.root_frame frame' current_frame in
      let steps = find_root root steps in
      let (last_frame, last_step as last), steps = find_valid steps in
      Ast_mapper.cache := last_step.ppx_cookie;
      Btype.backtrack last_step.snapshot;
      List.rev_scan_left steps ~f:(sync_frame catch) ~init:last
        (Parser.unroll_stack ~from:current_frame ~root:last_frame)
  with Not_found -> new_stack extensions catch current_frame

let sync_stack extensions catch steps current_frame =
  let result = sync_stack extensions catch steps current_frame in
  Typecore.reset_delayed_checks ();
  result

(* Compiler state *)

type state = {
  btype_cache : Btype.cache;
  env_cache   : Env.cache;
  extensions  : Extension.set;
  stamp : bool ref list;
}

let fluid_btype = Fluid.from_ref Btype.cache
let fluid_env = Fluid.from_ref Env.cache
let fluid_ast = Fluid.from_ref Ast_mapper.cache

(* Public API *)

type t = {
  state : state;
  steps : sync_step list;
}

let with_typer {state} f =
  Fluid.let' fluid_btype state.btype_cache @@ fun () ->
  Fluid.let' fluid_env state.env_cache     @@ fun () ->
  Fluid.let' fluid_ast (Ast_mapper.new_cache ()) @@ f

let protect_typer t f =
  with_typer t @@ fun () ->
  let caught = ref [] in
  Parsing_aux.catch_warnings caught @@ fun () ->
  Typing_aux.catch_errors caught    @@ fun () ->
  Fluid.let' Typing_aux.relax_typer false @@ fun () ->
  f caught

let dump ppf t =
  let ts = List.map t.steps ~f:(fun (_,x) -> x.raw) in
  List.iter (Raw_typer.dump ppf) ts

let fresh ~unit_name ~stamp extensions =
  let btype_cache = Btype.new_cache () in
  let env_cache = Env.new_cache ~unit_name in
  let state = { stamp; extensions; env_cache; btype_cache } in
  { state; steps = [] }

let get_value t = match t.steps with
  | (_,step) :: _ -> step
  | [] ->
    protect_typer t (empty t.state.extensions)

let update parser t =
  let steps =
    protect_typer t @@ fun caught ->
    sync_stack t.state.extensions caught t.steps
      (Parser.stack parser)
  in
  {t with steps}

let env t      = (get_value t).env
let exns t     = (get_value t).exns
let delayed_checks t =
  protect_typer t @@ fun exns ->
  let st = get_value t in
  let checks =
    List.fold_left ~f:(fun acc (_,typed,checks) ->
        try match typed with
          | `Fail _ -> acc
          | `Str str ->
            ignore (Includemod.signatures st.env
                      str.Typedtree.str_type
                      str.Typedtree.str_type : Typedtree.module_coercion);
            checks @ acc
          | `Sg sg ->
            ignore (Includemod.signatures st.env
                      sg.Typedtree.sig_type
                      sg.Typedtree.sig_type : Typedtree.module_coercion);
            checks @ acc
        with exn ->
          exns := exn :: !exns;
          acc
      ) ~init:[] st.contents
  in
  Typecore.delayed_checks := checks;
  Typecore.force_delayed_checks ();
  !exns

let contents t = (get_value t).contents
let extensions t = t.state.extensions

let is_valid t =
  List.for_all ~f:(!) t.state.stamp &&
  try with_typer t Env.check_cache_consistency
  with _exn -> false

let last_ident env = Raw_compat.last_ident (Env.summary env)

let to_browse contents =
  let with_env node =
    List.One (Browse_node.node_update_env Env.empty node, node)
  in
  let of_content (_,typed,_) = match typed with
    | `Fail _ -> None
    | `Sg sg -> Some (with_env (Browse_node.Signature sg))
    | `Str str -> Some (with_env (Browse_node.Structure str))
  in
  List.filter_map ~f:of_content contents

(** Heuristic to find suitable environment to complete / type at given position.
    1. Try to find environment near given cursor.
    2. Check if there is an invalid construct between found env and cursor :
      Case a.
        > let x = valid_expr ||
        The env found is the right most env from valid_expr, it's a correct
        answer.
      Case b.
        > let x = valid_expr
        > let y = invalid_construction||
        In this case, the env found is the same as in case a, however it is
        preferable to use env from enclosing module rather than an env from
        inside x definition.
 *)
let node_at ?(skip_recovered=false) typer pos_cursor =
  let structures = to_browse (contents typer) in
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | List.More ((_,node), (List.More ((_,node'), _) as ancestors))
      when Merlin_browse.is_recovered node' -> select ancestors
    | l -> l
  in
  match Merlin_browse.deepest_before pos_cursor structures with
  | Some path when skip_recovered -> select path
  | Some path -> path
  | None -> List.One (env typer, Browse_node.Dummy)
