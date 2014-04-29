open Std

let rec find_structure md =
  match md.Typedtree.mod_desc with
  | Typedtree.Tmod_structure _ -> Some md
  | Typedtree.Tmod_functor (_,_,_,md) -> find_structure md
  | Typedtree.Tmod_constraint (md,_,_,_) -> Some md
  | _ -> None

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
    raw: Raw_typer.t;
    snapshot: Btype.snapshot;
    env: Env.t;
    structures: Typedtree.structure list;
    exns: exn list;
  }

  let empty (extensions,catch) =
    let env = Env.initial_unsafe_string (*FIXME: should be in Raw_typer ?*) in
    let env = Env.open_pers_signature "Pervasives" env in
    let env = Extension.register extensions env in
    let raw = Raw_typer.empty in
    let exns = caught catch in
    let snapshot = Btype.snapshot () in
    { raw; snapshot; env; structures = []; exns }

  let validate _ t = Btype.is_valid t.snapshot

  let rewrite loc =
    let open Parsetree in
    function
    | Raw_typer.Functor_argument (id,mty) ->
      let mexpr = Pmod_structure [] in
      let mexpr = { pmod_desc = mexpr; pmod_loc = loc; pmod_attributes = [] } in
      let mexpr = Pmod_functor (id, mty, mexpr) in
      let mexpr = { pmod_desc = mexpr; pmod_loc = loc; pmod_attributes = [] } in
      failwith "TODO"
    (*let item = Pstr_module (Location.mknoloc "" , mexpr) in
      `fake { pstr_desc = item; pstr_loc = loc }*)
    | Raw_typer.Pattern (l,o,p) ->
      let expr = Pexp_constant (Asttypes.Const_int 0) in
      let expr = { pexp_desc = expr; pexp_loc = loc; pexp_attributes = [] } in
      let expr = Pexp_fun (l, o, p, expr) in
      let expr = { pexp_desc = expr; pexp_loc = loc; pexp_attributes = [] } in
      let item = Pstr_eval (expr,[]) in
      `fake { pstr_desc = item; pstr_loc = loc }
    | Raw_typer.Newtype s ->
      let expr = Pexp_constant (Asttypes.Const_int 0) in
      let expr = { pexp_desc = expr; pexp_loc = Location.none; pexp_attributes = [] } in
      let pat = { ppat_desc = Ppat_any; ppat_loc = Location.none; ppat_attributes = [] } in
      let expr = Pexp_fun ("", None, pat, expr) in
      let expr = { pexp_desc = expr; pexp_loc = Location.none; pexp_attributes = [] } in
      let expr = Parsetree.Pexp_newtype (s,expr) in
      let expr = { pexp_desc = expr; pexp_loc = loc; pexp_attributes = [] } in
      let item = Pstr_eval (expr,[]) in
      `fake { pstr_desc = item; pstr_loc = loc }
    | Raw_typer.Bindings (rec_,e) ->
      let item = Pstr_value (rec_,e) in
      `str [{ pstr_desc = item; pstr_loc = loc }]
    | Raw_typer.Open (override,name) ->
      let item = Pstr_open (Ast_helper.Opn.mk ~override name) in
      `str [{ pstr_desc = item; pstr_loc = loc }]
    | Raw_typer.Eval e ->
      `str [{
        Parsetree. pstr_desc = Parsetree.Pstr_eval (e,[]);
        pstr_loc = e.Parsetree.pexp_loc;
      }]
    | Raw_typer.Structure str ->
      `str str
    | Raw_typer.Signature sg ->
      `sg sg

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
            Either.get (Parsing_aux.catch_warnings (ref [])
                          (fun () -> Typemod.type_structure t.env [str] loc))
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
      {t with exns = exn :: caught catch @ t.exns;
              snapshot = Btype.snapshot () }

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

type t = {
  btype_cache: Btype.cache;
  env_cache: Env.cache;
  extensions : Extension.set;
  typer : I.t;
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

let fresh extensions =
  let btype_cache = Btype.new_cache () in
  let env_cache = Env.new_cache () in
  let result = protect_typer ~btype:btype_cache ~env:env_cache
      (fun exns -> I.empty (extensions,exns))
  in
  {
    typer = Either.get result;
    extensions; env_cache; btype_cache;
  }

let update parser t =
  let result =
    protect_typer ~btype:t.btype_cache ~env:t.env_cache
      (fun exns -> I.update' (t.extensions,exns) parser t.typer)
  in
  {t with typer = Either.get result}

let env t = (I.value t.typer).P.env
let structures t = (I.value t.typer).P.structures
let exns t = (I.value t.typer).P.exns
let extensions t = t.extensions

let is_valid t =
  match protect_typer ~btype:t.btype_cache ~env:t.env_cache
          (fun _ -> Env.check_cache_consistency ())
  with
  | Either.L _exn -> false
  | Either.R result -> result
