let (>>=) a f = match a with
  | Some a' -> f a'
  | None -> None

let union_loc_opt a b = match a,b with
  | None, None -> None
  | (Some _ as l), None | None, (Some _ as l) -> l
  | Some a, Some b -> Some (Location.union a b)

let rec signature_loc =
  let open Types in
  let rec mod_loc = function
    | Mty_ident _ -> None
    | Mty_functor (_,m1,m2) ->
        union_loc_opt (mod_loc m1) (mod_loc m2)
    | Mty_signature s ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
                        | (Some _ as v) -> v
                        | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
  in
  function
  | Sig_value (_,v)     -> Some v.val_loc
  | Sig_type (_,t,_)      -> Some t.type_loc
  | Sig_exception (_,e) -> Some e.exn_loc
  | Sig_modtype (_,Modtype_manifest m)
  | Sig_module (_,m,_)    -> mod_loc m
  | Sig_modtype (_,Modtype_abstract) -> None
  | Sig_class (_,_,_)
  | Sig_class_type (_,_,_) -> None

let signature_ident =
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_exception (i,_)
  | Sig_modtype (i,_)
  | Sig_module (i,_,_)
  | Sig_class (i,_,_)
  | Sig_class_type (i,_,_) -> i

let print_constructor ppf c =
  let open Types in
  match c.cstr_args with
  | [] ->
    Printtyp.type_expr ppf ({ level = 0 ; id = 0 ; desc = c.cstr_res.desc })
  | args ->
    let desc = Tarrow ("",{ level = 0; id = 0; desc = Ttuple args}, c.cstr_res,Cok) in
    Printtyp.type_expr ppf ({ level = 0 ; id = 0 ; desc  })

module Envs =
struct
  let summary_prev =
    let open Env in
    function
    | Env_empty -> None
    | Env_open (s,_) | Env_value (s,_,_)
    | Env_type (s,_,_) | Env_exception (s,_,_)
    | Env_module (s,_,_) | Env_modtype (s,_,_)
    | Env_class (s,_,_) | Env_cltype (s,_,_) ->
      Some s

  let signature_of_summary =
    let open Env in
    let open Types in
    function
    | Env_value (_,i,v)      -> Some (Sig_value (i,v))
    | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
    | Env_exception (_,i,e)  -> Some (Sig_exception (i,e))
    | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
    | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
    | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
    | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
    | Env_open _ | Env_empty -> None

  let summary_at pos sum =
    let cmp = Location.compare_pos pos in
    let rec aux sum =
      match signature_of_summary sum >>= signature_loc with
        | None -> summary_prev sum >>= aux
        | Some loc ->
      match cmp loc with
        | x when x < 0 -> None
        | 0 -> Some sum
        | x -> summary_prev sum >>= aux
    in
    aux sum

  let signature_of_env env =
    let open Types in
    let sg = ref [] in
    let append item = sg := item :: !sg in
    let rec aux summary =
      match summary with
      | Env.Env_empty -> ()
      (* Stop when encoutering extensions *)
      | Env.Env_module (_,i,_) when i = Extensions.ident -> ()
      | Env.Env_value (s,i,v) ->
          append (Sig_value (i,v));
          aux s
      | Env.Env_type (s,i,t) ->
          append (Sig_type (i,t,Trec_not)); (* Trec_not == bluff, FIXME *)
          aux s
      | Env.Env_exception (s,i,e) ->
          append (Sig_exception (i,e));
          aux s
      | Env.Env_module (s,i,m) ->
          append (Sig_module (i,m,Trec_not));
          aux s
      | Env.Env_modtype (s,i,mt) ->
          append (Sig_modtype (i,mt));
          aux s
      | Env.Env_class (s,i,c) ->
          append (Sig_class (i,c,Trec_not));
          aux s
      | Env.Env_cltype (s,i,ct) ->
          append (Sig_class_type (i,ct,Trec_not));
          aux s
      | Env.Env_open (s,p) ->
          aux s
    in
    let summary = Env.summary env in
    aux summary;
    Typemod.simplify_signature (!sg)

  open Typedtree
  type kind =
    | Type of Types.type_declaration
    | Expr of Types.type_expr
    | Module of Types.module_type
    | Modtype of Types.modtype_declaration
    | Class of Ident.t * Types.class_declaration
    | ClassType of Ident.t * Types.class_type_declaration
    | Other

  type t = T of Location.t * Env.t * kind * t list Lazy.t

  let cmp_start (T (l1,_,_,_)) (T (l2,_,_,_)) =
    Misc.compare_pos l1.Location.loc_start l2.Location.loc_end

  let singleton ?(kind=Other) l e = T (l,e,kind,lazy [])

  let rec structure { str_final_env ; str_items } =
    List.map (structure_item ~env:str_final_env) str_items

  and structure_item ~env { str_desc ; str_loc ; str_env } =
    T (str_loc, str_env, Other, lazy (structure_item_desc ~env str_desc))

  and structure_item_desc ~env = function
    | Tstr_eval e            -> [expression e]
    | Tstr_value (_,pes)     -> patterns ~env:env pes
    | Tstr_primitive (_,l,_)
    | Tstr_exception (_,l,_) -> [singleton l.Location.loc env]
    | Tstr_module (_,_,m)    -> [module_expr m]
    | Tstr_recmodule ms      -> List.map (fun (_,_,_,m) -> module_expr m) ms
    | Tstr_type ilds ->
      List.map
        (fun (_,l,{typ_type}) -> singleton ~kind:(Type typ_type) l.Location.loc env)
        ilds
    | Tstr_modtype (_,l,_)
    | Tstr_exn_rebind (_,l,_,_) -> [singleton l.Location.loc env]
    | Tstr_open _               -> []
    | Tstr_class lst            -> List.map (class_declaration ~env) lst
    | Tstr_class_type lst ->
      List.map
        (fun (id,l,{ ci_type_decl }) ->
          singleton ~kind:(ClassType (id, ci_type_decl)) l.Location.loc env)
        lst
    | Tstr_include (m,_) -> [module_expr m]

  and class_declaration ~env (cd, _, _virtual_flag) =
    let kind = Class (cd.ci_id_class, cd.ci_decl) in (* FIXME: use [ci_id_object] ? *)
    match cd.ci_expr.cl_desc with
    | Tcl_structure class_struct ->
      let children = lazy (class_structure ~env class_struct) in
      T (cd.ci_loc, env, kind, children)
    (* TODO: extend *)
    | _ -> singleton ~kind cd.ci_loc env

  and class_structure ~env class_struct =
    let pat = (* where is that pattern in the concret syntax? *)
      let kind = Expr class_struct.cstr_pat.pat_type in
      singleton ~kind class_struct.cstr_pat.pat_loc env
    in
    let fields = Misc.list_filter_map (class_field ~env) class_struct.cstr_fields in
    pat :: fields

  and class_field ~env { cf_desc ; cf_loc } =
    match cf_desc with
    | Tcf_val (_, _, _, _, kind, _)
    | Tcf_meth (_, _, _, kind, _) ->
      begin match kind with
      | Tcfk_concrete e -> Some (expression e)
      | _ -> None
      end
    | _ -> None


  and patterns ?expr ?env pes = List.fold_left
    begin fun ls (p,e) ->
      let l =
        singleton ~kind:(Expr p.pat_type) p.pat_loc
          (match env with Some p -> p | _ -> e.exp_env)
      in
      l :: expression e :: ls
    end [] pes

  and pattern expr env { pat_loc } = singleton ~kind:(Expr expr.exp_type) pat_loc env

  and expression_extra t = function
    | (Texp_open (_,_,e),l) -> T (l, e, Other, lazy [t])
    | _ -> t

  and expression { exp_desc ; exp_loc ; exp_extra ; exp_type ; exp_env } =
    List.fold_left expression_extra
      (T (exp_loc, exp_env, (Expr exp_type), lazy (expression_desc exp_desc)))
      exp_extra

  and expression_desc = function
    | Texp_ident (_,_,_) -> []
    | Texp_constant _ -> []
    | Texp_let (_,pes,e) -> expression e :: patterns ~env:e.exp_env pes
    | Texp_function (_,pes,_) -> patterns pes
    | Texp_apply (e,leso) ->
      let helper = function (_,Some e,_) -> Some (expression e) | _ -> None in
      expression e :: Misc.list_filter_map helper leso
    | Texp_match (e,pes,_) -> expression e :: patterns pes
    | Texp_try (e,pes) -> expression e :: patterns pes
    | Texp_tuple (es) -> List.map expression es
    | Texp_construct (_,_,_,es,_) -> List.map expression es
    | Texp_variant (_,Some e) -> [expression e]
    | Texp_variant (_,None) -> []
    | Texp_record (pldes,Some e) -> expression e :: List.map (fun (_,_,_,e) -> expression e) pldes
    | Texp_record (pldes,None) -> List.map (fun (_,_,_,e) -> expression e) pldes
    | Texp_array es -> List.map expression es
    | Texp_assert ea
    | Texp_lazy ea
    | Texp_setinstvar (_,_,_,ea)
    | Texp_send (ea, _, None)
    | Texp_field (ea,_,_,_) -> [expression ea]
    | Texp_ifthenelse (ea,eb,None)
    | Texp_setfield (ea,_,_,_,eb)
    | Texp_sequence (ea,eb)
    | Texp_when (ea,eb)
    | Texp_send (ea, _, Some eb)
    | Texp_while (ea,eb) -> [expression ea ; expression eb]
    | Texp_for (_,_,ea,eb,_,ec)
    | Texp_ifthenelse (ea,eb,Some ec) -> List.map expression [ea;eb;ec]
    | Texp_override (_,ples) -> List.map (fun (_,_,e) -> expression e) ples
    | Texp_letmodule (_,_,m,e) -> [expression e ; module_expr m ]
    | Texp_assertfalse -> []
    | Texp_pack m -> [module_expr m]
    | Texp_new _
    | Texp_instvar _
    | Texp_object _ -> []

  and module_expr { mod_env ; mod_desc ; mod_type ; mod_loc } =
    T (mod_loc, mod_env, (Module mod_type), lazy (module_expr_desc mod_desc))

  and module_expr_desc = function
    | Tmod_ident _ -> []
    | Tmod_structure s -> structure s
    | Tmod_constraint (e,_,_,_)
    | Tmod_functor (_,_,_,e) -> [module_expr e]
    | Tmod_apply (e1,e2,_) -> [module_expr e1 ; module_expr e2]
    | Tmod_unpack (e,_) -> [expression e]
end

let browse_local_near pos nodes =
  let best_of (Envs.T (l,_,_,_) as t) (Envs.T (l',_,_,_) as t') =
    if Misc.compare_pos l.Location.loc_end l'.Location.loc_end < 0
    then t'
    else t
  in
  let cmp = Location.compare_pos pos in
  List.fold_left
  begin fun best (Envs.T (loc,_,_,_) as t) ->
    match cmp loc, best with
      | n, _ when n < 0 -> best
      | n, None -> Some t
      | n, Some t' -> Some (best_of t t')
  end None nodes

let browse_near pos envs =
  let rec traverse (Envs.T (loc,env,t,lazy children)) =
    match browse_local_near pos children with
      | Some t' -> traverse t'
      | None -> (loc,env,t)
  in
  match browse_local_near pos envs with
    | Some t -> Some (traverse t)
    | None -> None

let browse_enclosing pos envs =
  let not_enclosing (Envs.T (loc,_,_,_)) =
    not (Location.compare_pos pos loc = 0)
  in
  let rec traverse (Envs.T (loc,env,t,lazy children)) results =
    match browse_local_near pos children with
      | Some t' -> traverse t' (t' :: results)
      | None -> results
  in
  match browse_local_near pos envs with
    | None -> []
    | Some t ->
        let results = traverse t [t] in
        Misc.list_drop_while not_enclosing results

let rec dump_envs envs =
  let dump_env (Envs.T (l,_,k,lazy children)) =
    let kind = match k with
      | Envs.Type _ -> "type"
      | Envs.Expr _ -> "expr"
      | Envs.Module _ -> "module"
      | Envs.Modtype _ -> "modtype"
      | Envs.Class (_, _) -> "class"
      | Envs.ClassType _ -> "class_type"
      | Envs.Other -> "??"
    in
    Protocol.with_location l
    [
      "kind", `String kind;
      "children", dump_envs children
    ]
  in
  let envs = List.sort Envs.cmp_start envs in
  `List (List.map dump_env envs)
