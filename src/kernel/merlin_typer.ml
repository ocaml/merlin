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

module P = struct
  open Raw_parser

  type st = Extension.set * exn list ref

  type t = {
    snapshot: Btype.snapshot;
    env: Env.t;
    structures: Typedtree.structure list;
    exns: exn list;
  }

  let empty (extensions,catch) =
    let env = Env.initial in
    let env = Env.open_pers_signature "Pervasives" env in
    let env = Extension.register extensions env in
    { snapshot = Btype.snapshot (); env; structures = []; exns = caught catch }

  let validate _ t = Btype.is_valid t.snapshot

  let frame (_,catch) f t =
    let mkeval e = {
      Parsetree. pstr_desc = Parsetree.Pstr_eval e;
      pstr_loc = e.Parsetree.pexp_loc;
    } in
    let case =
      match Merlin_parser.value f with
      | Terminal _ | Bottom -> `none
      | Nonterminal nt ->
      match nt with
      | NT'structure str | NT'structure_tail str | NT'structure_item str ->
        `str str
      | NT'top_expr e | NT'strict_binding e | NT'simple_expr e | NT'seq_expr e
      | NT'opt_default (Some e) | NT'fun_def e | NT'fun_binding e | NT'expr e
      | NT'match_action e | NT'labeled_simple_expr (_,e) | NT'label_ident (_,e)
      | NT'label_expr (_,e) ->
        `str [mkeval e]
      | NT'expr_semi_list el | NT'expr_comma_opt_list el
      | NT'expr_comma_list el  ->
        `str (List.map ~f:mkeval el)
      | NT'module_expr pmod | NT'module_binding pmod ->
        `md pmod
      | NT'signature_item sg ->
        `sg sg
      | NT'signature sg ->
        `sg (List.rev sg)
      | NT'module_functor_arg (id,mty) ->
        `fmd (id,mty)
      | NT'labeled_simple_pattern pat ->
        `pat pat
      | _ -> `none
    in
    match case with
    | `none -> t
    | _ as case ->
    try
      Btype.backtrack t.snapshot;
      let env, structures =
        match case with
        | `str str ->
          let structures,_,env = Typemod.type_structure t.env str Location.none in
          env, structures :: t.structures
        | `sg sg ->
          let sg = Typemod.transl_signature t.env sg in
          let sg = sg.Typedtree.sig_type in
          Env.add_signature sg t.env, t.structures
        | `md pmod ->
          let tymod = Typemod.type_module t.env pmod in
          begin match find_structure tymod with
            | None -> t.env
            | Some md -> md.Typedtree.mod_env
          end, t.structures
        | `fmd (id,mty) ->
          let mexpr = Parsetree.Pmod_structure [] in
          let mexpr = { Parsetree. pmod_desc = mexpr; pmod_loc = Location.none } in
          let mexpr = Parsetree.Pmod_functor (id, mty, mexpr) in
          let mexpr = { Parsetree. pmod_desc = mexpr; pmod_loc = Location.none } in
          let tymod = Typemod.type_module t.env mexpr in
          begin match find_structure tymod with
            | None -> t.env
            | Some md -> md.Typedtree.mod_env
          end, t.structures
        | `pat (l,o,p) ->
          let expr = Parsetree.Pexp_constant (Asttypes.Const_int 0) in
          let expr = { Parsetree. pexp_desc = expr; pexp_loc = Location.none } in
          let expr = Parsetree.Pexp_function (l, o, [p, expr]) in
          let expr = { Parsetree. pexp_desc = expr; pexp_loc = Location.none } in
          begin match Typecore.type_expression t.env expr with
            | {Typedtree. exp_desc = Typedtree.Texp_function (_,[_,{Typedtree.exp_env}],_)} ->
              exp_env, t.structures
            | _ -> assert false
          end
        | `none -> t.env, t.structures
      in
      {env; structures; snapshot = Btype.snapshot ();
       exns = caught catch @ t.exns}
    with exn ->
      {t with exns = exn :: caught catch @ t.exns}

  let delta st f t ~old:_ = frame st f t
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
  Either.join (Merlin_parsing.catch_warnings caught >>= fun () ->
               Merlin_types.catch_errors caught >>= fun () ->
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

let is_valid t =
  let env = env t in
  match protect_typer ~btype:t.btype_cache ~env:t.env_cache
          (fun _ -> Env.check_cache_consistency ())
  with
  | Either.L _exn -> false
  | Either.R result -> result
