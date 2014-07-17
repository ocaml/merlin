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
    structures : Typedtree.structure list;
    exns       : exn list;
  }

  let empty (extensions,catch) =
    let env = Raw_typer.fresh_env () in
    let env = Env.open_pers_signature "Pervasives" env in
    let env = Extension.register extensions env in
    let raw = Raw_typer.empty in
    let exns = caught catch in
    let snapshot = Btype.snapshot () in
    { raw; snapshot; env; structures = []; exns }

  let validate _ t = Btype.is_valid t.snapshot

  let append catch loc item t =
    try
      Btype.backtrack t.snapshot;
      let env, structures =
        match item with
        | `str str ->
          let structure,_,env = Typemod.type_structure t.env str loc in
          env, structure :: t.structures
        | `sg sg ->
          let sg = Typemod.transl_signature t.env sg in
          let sg = sg.Typedtree.sig_type in
          Env.add_signature sg t.env, t.structures
        | `fake str ->
          let structure,_,_ =
            Parsing_aux.catch_warnings (ref []) @@ fun () ->
            Typemod.type_structure t.env [str] loc
          in
          let browse =
            BrowseT.of_node ~loc ~env:t.env (BrowseT.Structure structure)
          in
          (last_env browse).BrowseT.t_env, structure :: t.structures
        | `none -> t.env, t.structures
      in
      Typecore.reset_delayed_checks ();
      {env; structures; snapshot = Btype.snapshot (); raw = t.raw;
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

  let delta st f t ~old:_ = frame st f t

  let evict st _ = ()

end

module I = Merlin_parser.Integrate (P)

type kind =
  | Incremental of I.t
  | Manual of P.t

type t = {
  btype_cache : Btype.cache;
  env_cache   : Env.cache;
  extensions  : Extension.set;
  typer       : kind;
  stamp       : bool ref;
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
  let result = protect_typer ~btype:btype_cache ~env:env_cache @@ fun exns ->
    Either.try' @@ fun () ->
    I.empty (extensions,exns)
  in
  {
    stamp;
    typer = Incremental (Either.get result);
    extensions; env_cache; btype_cache;
  }

let get_incremental state = function
  | Manual _ -> I.empty state
  | Incremental i -> i

let get_value = function
  | Incremental i -> I.value i
  | Manual v -> v

let update parser t =
  let result =
    protect_typer ~btype:t.btype_cache ~env:t.env_cache @@ fun exns ->
    Either.try' @@ fun () ->
    let state = (t.extensions,exns) in
    I.update' state parser (get_incremental state t.typer)
  in
  {t with typer = Incremental (Either.get result)}

let env t = (get_value t.typer).P.env
let structures t = (get_value t.typer).P.structures
let exns t = (get_value t.typer).P.exns
let extensions t = t.extensions

let is_valid t =
  !(t.stamp) &&
  match
    protect_typer ~btype:t.btype_cache ~env:t.env_cache @@ fun _ ->
    Either.try' Env.check_cache_consistency
  with
  | Either.L _exn -> false
  | Either.R result -> result

let dump ppf t =
  let ts = match t.typer with
    | Manual v -> [v]
    | Incremental i ->
      let ls = i :: List.unfold I.previous i in
      List.map ~f:I.value ls
  in
  let ts = List.map ts ~f:(fun x -> x.P.raw) in
  List.iter (Raw_typer.dump ppf) ts

let manual t item =
  let typer =
    protect_typer ~btype:t.btype_cache ~env:t.env_cache @@ fun exns ->
    Either.try' @@ fun () ->
    let p = P.empty (t.extensions,exns) in
    P.append exns Location.none item p
  in
  {t with typer = Manual (Either.get typer)}

let with_typer t f =
  Fluid.let' fluid_btype t.btype_cache @@ fun () ->
  Fluid.let' fluid_env t.env_cache @@ fun () ->
  f t
