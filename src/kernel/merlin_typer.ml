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
module Parser = Merlin_parser

(* All exceptions are added to an [exn list ref] referred to as "caught" in the
   rest of the file.
   This helper just extracs exceptions and reset the list. *)

let mk_arg_lbl = Raw_compat.Parsetree.arg_label_of_str

let caught catch =
  let caught = !catch in
  catch := [];
  caught

type content =
  [ `Str of Typedtree.structure
  | `Sg of Typedtree.signature
  | `Fail of Env.t * Location.t
  ]

(* Intermediate, resumable type checking state *)

type step = {
  raw        : Raw_typer.t;
  ppx_cookie : Ast_mapper.cache;
  snapshot   : Btype.snapshot;
  env        : Env.t;
  contents   : (content * Typecore.delayed_check list) list;
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

let rewrite_raw loc = function
  | Raw_typer.Functor_argument (id,mty) ->
    let mexpr = Ast_helper.Mod.structure ~loc [] in
    let mexpr = Ast_helper.Mod.functor_ ~loc id mty mexpr in
    let mb = Ast_helper.Mb.mk (Location.mknoloc "") mexpr in
    `fake [Ast_helper.Str.module_ ~loc mb]
  | Raw_typer.Pattern (l,o,p) ->
    let expr = Ast_helper.Exp.constant ~loc (Asttypes.Const_int 0) in
    let expr = Ast_helper.Exp.fun_ ~loc l o p expr in
    `fake [Ast_helper.Str.eval ~loc expr]
  | Raw_typer.Newtype s ->
    let expr = Ast_helper.Exp.constant (Asttypes.Const_int 0) in
    let patt = Ast_helper.Pat.any () in
    let expr = Ast_helper.Exp.fun_ (mk_arg_lbl "") None patt expr in
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
  | `str str -> `str (Pparse.apply_rewriters_str ~tool_name:"merlin" str)
  | `sg sg -> `sg (Pparse.apply_rewriters_sig ~tool_name:"merlin" sg)
  | `fake str -> `fake (Pparse.apply_rewriters_str ~tool_name:"merlin" str)

let rewrite loc raw = rewrite_ppx (rewrite_raw loc raw)

(* Produce a new step by processing one frame from the parser *)

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

let append catch loc step item =
  try
    Typecore.delayed_checks := [];
    Fluid.set Typing_aux.relax_typer false;
    let env, contents =
      match item with
      | `str str ->
        let structure,t,env = Typemod.type_structure step.env str loc in
        env,
        (`Str structure, !Typecore.delayed_checks) :: step.contents
      | `sg sg ->
        let sg = Typemod.transl_signature step.env sg in
        sg.Typedtree.sig_final_env,
        (`Sg sg, !Typecore.delayed_checks) :: step.contents
      | `fake str ->
        let structure,_,_ =
          Parsing_aux.catch_warnings (ref [])
            (fun () -> Typemod.type_structure step.env str loc)
        in
        let browse =
          BrowseT.of_node ~loc ~env:step.env (BrowseT.Structure structure)
        in
        (last_env browse).BrowseT.t_env,
        (`Str structure, !Typecore.delayed_checks) :: step.contents
      | `none -> step.env, step.contents
    in
    let snapshot = Btype.snapshot () in
    let ppx_cookie = !Ast_mapper.cache in
    {env; contents; snapshot; ppx_cookie;
     raw = step.raw;
     exns = caught catch @ step.exns}
  with exn ->
    let snapshot = Btype.snapshot () in
    {step with snapshot;
               exns = exn :: caught catch @ step.exns;
               contents = (`Fail (step.env, loc), []) :: step.contents}

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
    List.fold_left ~f:(fun acc (content,checks) ->
        try match content with
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
          | `Fail _ -> acc
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
