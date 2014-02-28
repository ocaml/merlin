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

module Last_env = struct
  open Typedtree

  let rec structure { str_final_env = env; str_items = l } =
    Option.value_map (List.last l)
      ~f:structure_item
      ~default:env

  and structure_item { str_desc; str_env } =
    structure_item_desc str_env str_desc

  and structure_item_desc env = function
    | Tstr_eval e -> expression e
    | Tstr_value (_, l) ->
      Option.value_map (List.last l)
        ~f:(fun (_,e) -> expression e)
        ~default:env
    | Tstr_primitive (_,_,v) -> value_description v
    | Tstr_exception (_,_,e) -> exception_declaration env e
    | Tstr_module (_,_,m) -> module_expr m
    | Tstr_recmodule l ->
      Option.value_map (List.last l)
        ~f:(fun (_,_,_,m) -> module_expr m)
        ~default:env
    | Tstr_modtype (_,_,mt) -> module_type mt
    | Tstr_class l ->
      Option.value_map (List.last l)
        ~f:(fun (c,_,_) -> class_declaration c)
        ~default:env
    | Tstr_class_type l ->
      Option.value_map (List.last l)
        ~f:(fun (_,_,c) -> class_type_declaration c)
        ~default:env
    | Tstr_include (m,_) -> module_expr m
    | Tstr_type _ | Tstr_open _ | Tstr_exn_rebind _ -> env

  and expression { exp_desc; exp_env } = expression_desc exp_env exp_desc

  and expression_desc env = function
    | Texp_ident _ | Texp_instvar _ | Texp_constant _ | Texp_apply _
    | Texp_assertfalse -> env
    | Texp_let (_,_,e) -> expression e
    | Texp_function (_,l,_) ->
      Option.value_map (List.last l)
        ~f:(fun (_,e) -> expression e)
        ~default:env
    | Texp_try (e,l) | Texp_match (e,l,_) ->
      expression (Option.value_map (List.last l) ~f:snd ~default:e)
    | Texp_array l | Texp_tuple l ->
      Option.value_map (List.last l)
        ~f:expression
        ~default:env
    | Texp_construct (_,_,l,_) ->
      Option.value_map (List.last l)
        ~f:expression
        ~default:env
    | Texp_variant (_,e) ->
      Option.value_map e
        ~f:expression
        ~default:env
    | Texp_record (l,_) ->
      Option.value_map (List.last l)
        ~f:(fun (_,_,e) -> expression e)
        ~default:env
    | Texp_field (e,_,_) | Texp_setfield (_,_,_,e) | Texp_ifthenelse (_,_,Some e)
    | Texp_ifthenelse (_,e,None) | Texp_sequence (_,e) | Texp_while (_,e)
    | Texp_for (_,_,_,_,_,e) | Texp_when (_,e) | Texp_send (_,_,Some e)
    | Texp_send (e,_,None) | Texp_letmodule (_,_,_,e) | Texp_lazy e
    | Texp_assert e | Texp_setinstvar (_,_,_,e) -> expression e
    | Texp_new (_,_,c) -> failwith "TODO"
    | Texp_override (_,l) ->
      Option.value_map (List.last l)
        ~f:(fun (_,_,e) -> expression e)
        ~default:env
    | Texp_object (e,_) -> failwith "TODO"
    | Texp_pack m -> module_expr m

  and value_description { val_desc } = core_type val_desc

  and core_type { ctyp_env } = ctyp_env

  and exception_declaration env { exn_params } =
    Option.value_map (List.last exn_params)
      ~f:core_type
      ~default:env

  and module_expr { mod_env; mod_desc } = module_expr_desc mod_env mod_desc

  and module_expr_desc env = function
    | Tmod_ident _ -> env
    | Tmod_structure s -> structure s
    | Tmod_constraint (m,_,_,_) | Tmod_apply (_,m,_) | Tmod_functor (_,_,_,m) -> module_expr m
    | Tmod_unpack (e,_) -> expression e

  and module_type { mty_env; mty_desc } = module_type_desc mty_env mty_desc

  and module_type_desc env = function
    | Tmty_signature _ | Tmty_ident _ -> env
    | Tmty_with (m,_) | Tmty_functor (_,_,_,m) -> module_type m
    | Tmty_typeof m -> module_expr m

  and class_infos f { ci_expr } = f ci_expr

  and class_expr { cl_env; cl_desc } = class_expr_desc cl_env cl_desc

  and class_expr_desc env = function
    | Tcl_ident (_,_,l) ->
    Option.value_map (List.last l)
      ~f:core_type
      ~default:env
    | Tcl_structure c -> failwith "TODO"
    | Tcl_fun (_,_,_,e,_) | Tcl_apply (e,_) | Tcl_let (_,_,_,e)
    | Tcl_constraint (e,_,_,_,_) -> class_expr e

  and class_declaration _ = failwith "TODO"

  and class_type_declaration _ = failwith "TODO"
end

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
    let loc = Merlin_parser.location f in
    let case =
      match Merlin_parser.value f with
      | Terminal _ | Bottom -> `none
      | Nonterminal nt ->
      match nt with
      | NT'structure str | NT'structure_tail str | NT'structure_item str ->
        `str str
      | NT'mod_open o -> `Open o
      | NT'top_expr e | NT'strict_binding e | NT'simple_expr e | NT'seq_expr e
      | NT'opt_default (Some e) | NT'fun_def e | NT'fun_binding e | NT'expr e
      | NT'match_action e | NT'labeled_simple_expr (_,e) | NT'label_ident (_,e)
      | NT'label_expr (_,e) | NT'constrained_seq_expr e ->
        `str [mkeval e]
      | NT'let_bindings e ->
        let rec_ =
          match Option.map ~f:Merlin_parser.value (Merlin_parser.next f) with
          | Some (Nonterminal (NT'rec_flag r)) -> r
          | None | Some _ -> Asttypes.Nonrecursive
        in
        `binds (rec_,e)
      (*| NT'let_rec_bindings e -> `binds e*)
      | NT'expr_semi_list el | NT'expr_comma_opt_list el
      | NT'expr_comma_list el  ->
        `str (List.map ~f:mkeval el)
      | NT'new_type s -> `nty s
      | NT'signature_item sg ->
        `sg sg
      | NT'signature sg ->
        `sg (List.rev sg)
      | NT'module_functor_arg (id,mty) ->
        `fmd (id,mty)
      | NT'labeled_simple_pattern pat ->
        `pat pat
      (* Approximation to match/with typing: we only introduce names in the environment, not taking the matched expression into account *)
      | NT'pattern pat ->
        `pat ("",None,pat)
      | _ -> `none
    in
    let case =
      let open Parsetree in
      match case with
      | `fmd (id,mty) ->
          let mexpr = Pmod_structure [] in
          let mexpr = { pmod_desc = mexpr; pmod_loc = loc } in
          let mexpr = Pmod_functor (id, mty, mexpr) in
          let mexpr = { pmod_desc = mexpr; pmod_loc = loc } in
          let item = Pstr_module (Location.mknoloc "" , mexpr) in
          `fake { pstr_desc = item; pstr_loc = loc }
      | `pat (l,o,p) ->
        let expr = Pexp_constant (Asttypes.Const_int 0) in
        let expr = { pexp_desc = expr; pexp_loc = loc } in
        let expr = Pexp_function (l, o, [p, expr]) in
        let expr = { pexp_desc = expr; pexp_loc = loc } in
        let item = Pstr_eval expr in
        `fake { pstr_desc = item; pstr_loc = loc }
      | `nty s ->
        let expr = Pexp_constant (Asttypes.Const_int 0) in
        let expr = { pexp_desc = expr; pexp_loc = Location.none } in
        let pat = { ppat_desc = Ppat_any; ppat_loc = Location.none } in
        let expr = Pexp_function ("", None, [pat, expr]) in
        let expr = { pexp_desc = expr; pexp_loc = Location.none } in
        let expr = Parsetree.Pexp_newtype (s,expr) in
        let expr = { pexp_desc = expr; pexp_loc = loc } in
        let item = Pstr_eval expr in
        `fake { pstr_desc = item; pstr_loc = loc }
      | `binds (rec_,e) ->
        let item = Pstr_value (rec_,e) in
        `str [{ pstr_desc = item; pstr_loc = loc }]
      | `Open (flag,name) ->
        let item = Pstr_open (flag,name) in
        `str [{ pstr_desc = item; pstr_loc = loc }]
      | (`sg _ | `str _ | `none) as case -> case
    in
    match case with
    | `none -> t
    | _ as case ->
    try
      Btype.backtrack t.snapshot;
      let env, structures =
        match case with
        | `str str ->
          let structure,_,env = Typemod.type_structure t.env str loc in
          env, structure :: t.structures
        | `sg sg ->
          let sg = Typemod.transl_signature t.env sg in
          let sg = sg.Typedtree.sig_type in
          Env.add_signature sg t.env, t.structures
        | `fake str ->
          let structure,_,_ =
            Either.get (Merlin_parsing.catch_warnings (ref [])
              (fun () -> Typemod.type_structure t.env [str] loc))
          in
          Last_env.structure structure, structure :: t.structures
        | `none -> t.env, t.structures
      in
      Typecore.reset_delayed_checks ();
      {env; structures; snapshot = Btype.snapshot ();
       exns = caught catch @ t.exns}
    with exn ->
      Typecore.reset_delayed_checks ();
      {t with exns = exn :: caught catch @ t.exns}

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
  match protect_typer ~btype:t.btype_cache ~env:t.env_cache
          (fun _ -> Env.check_cache_consistency ())
  with
  | Either.L _exn -> false
  | Either.R result -> result
