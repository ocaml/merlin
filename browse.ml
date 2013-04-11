open Typedtree

type context =
  | Type of Types.type_declaration
  | Expr of Types.type_expr
  | Module of Types.module_type
  | Modtype of Types.modtype_declaration
  | Class of Ident.t * Types.class_declaration
  | ClassType of Ident.t * Types.class_type_declaration
  | Other

(* Typedtree navigation made easy *)
type t = {
  loc : Location.t;
  env : Env.t;
  context : context;
  nodes : t list Lazy.t
}

let singleton ?(context=Other) loc env = 
  { loc ; env ; context ; nodes = lazy [] }

let rec structure { str_final_env ; str_items } =
  List.map (structure_item ~env:str_final_env) str_items

and structure_item ~env { str_desc ; str_loc ; str_env } =
  { loc = str_loc ; env = str_env ; context = Other ;
    nodes = lazy (structure_item_desc ~env str_desc) }

and structure_item_desc ~env = function
  | Tstr_eval e            -> [expression e]
  | Tstr_value (_,pes)     -> patterns ~env:env pes
  | Tstr_primitive (_,l,_)
  | Tstr_exception (_,l,_) -> [singleton l.Location.loc env]
  | Tstr_module (_,_,m)    -> [module_expr m]
  | Tstr_recmodule ms      -> List.map (fun (_,_,_,m) -> module_expr m) ms
  | Tstr_type ilds ->
    let aux (_,l,{typ_type}) =
      singleton ~context:(Type typ_type)
                l.Location.loc env
    in
    List.map aux ilds
  | Tstr_modtype (_,l,_)
  | Tstr_exn_rebind (_,l,_,_) -> [singleton l.Location.loc env]
  | Tstr_open _               -> []
  | Tstr_class lst            -> List.map (class_declaration ~env) lst
  | Tstr_class_type lst ->
    List.map
      (fun (id,l,{ ci_type_decl }) ->
        singleton ~context:(ClassType (id, ci_type_decl)) l.Location.loc env)
      lst
  | Tstr_include (m,_) -> [module_expr m]

and class_declaration ~env (cd, _, _virtual_flag) =
  let context = Class (cd.ci_id_class, cd.ci_decl) in
  (* FIXME: use [ci_id_object] ? *)
  match cd.ci_expr.cl_desc with
  | Tcl_structure class_struct ->
    let nodes = lazy (class_structure ~env class_struct) in
    { loc = cd.ci_loc ; env ; context ; nodes }
  (* TODO: extend *)
  | _ -> singleton ~context cd.ci_loc env

and class_structure ~env class_struct =
  let pat = (* where is that pattern in the concret syntax? *)
    let context = Expr class_struct.cstr_pat.pat_type in
    singleton ~context class_struct.cstr_pat.pat_loc env
  in
  let fields = Misc.list_filter_map (class_field ~env) class_struct.cstr_fields in
  pat :: fields

and class_field ~env { cf_desc ; cf_loc } =
  match cf_desc with
  | Tcf_val (_, _, _, _, context, _)
  | Tcf_meth (_, _, _, context, _) ->
    begin match context with
    | Tcfk_concrete e -> Some (expression e)
    | _ -> None
    end
  | _ -> None

and patterns ?expr ?env pes = List.fold_left
  begin fun ls (p,e) ->
    let l =
      singleton ~context:(Expr p.pat_type) p.pat_loc
        (match env with Some p -> p | _ -> e.exp_env)
    in
    l :: expression e :: ls
  end [] pes

and pattern expr env { pat_loc } =
  singleton ~context:(Expr expr.exp_type) pat_loc env

and expression_extra t = function
  | (Texp_open (_,_,env),loc) -> { loc ; env ; context = Other ; nodes = lazy [t] }
  | _ -> t

and expression { exp_desc ; exp_loc ; exp_extra ; exp_type ; exp_env } =
  List.fold_left expression_extra
    { loc = exp_loc ; env = exp_env ; context = Expr exp_type ;
      nodes = lazy (expression_desc exp_desc) }
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
  { loc = mod_loc ; env = mod_env ; context = Module mod_type ;
    nodes = lazy (module_expr_desc mod_desc) }

and module_expr_desc = function
  | Tmod_ident _ -> []
  | Tmod_structure s -> structure s
  | Tmod_constraint (e,_,_,_)
  | Tmod_functor (_,_,_,e) -> [module_expr e]
  | Tmod_apply (e1,e2,_) -> [module_expr e1 ; module_expr e2]
  | Tmod_unpack (e,_) -> [expression e]

let local_near pos nodes =
  let cmp = Location.compare_pos pos in
  let best_of ({ loc = l1 } as t) ({ loc = l2 } as t') =
    match cmp l2, cmp l1 with
    | 0, 0 ->
      (* Cursor is inside locations: select smaller one *)
      if Misc.compare_pos l1.Location.loc_end l2.Location.loc_end < 0
      then t'
      else t
      (* Cursor inside one location, prefer it *)
    | 0, _ -> t'
    | _, 0 -> t
    | _, _ ->
      (* Cursor outside locations, select the rightmost one *)
      if Misc.compare_pos l1.Location.loc_end l2.Location.loc_end < 0
      then t
      else t'
  in
  List.fold_left
  begin fun best t ->
    match cmp t.loc, best with
    | n, _ when n < 0 -> best
    | n, None -> Some t
    | n, Some t' -> Some (best_of t t')
  end None nodes

let is_enclosing pos { loc } =
  (Location.compare_pos pos loc = 0)

let traverse_branch pos tree =
  let rec traverse { nodes = lazy nodes } acc =
    match local_near pos nodes with
    | Some t' -> traverse t' (t' :: acc) 
    | None -> acc
  in
  traverse tree [tree]

let deepest_before pos envs =
  match local_near pos envs with
  | None -> None
  | Some t -> Some (List.hd (traverse_branch pos t))

let nearest_before pos envs =
  match local_near pos envs with
  | None -> None
  | Some t -> 
    let rec aux = function
      | a :: b :: tail when is_enclosing pos b -> Some a
      | [x] -> Some x
      | [] -> None
      | _ :: tail -> aux tail
    in
    aux (traverse_branch pos t)

let enclosing pos envs =
  let not_enclosing l = not (is_enclosing pos l) in
  match local_near pos envs with
  | None -> []
  | Some t ->
    let results = traverse_branch pos t in
    Misc.list_drop_while not_enclosing results
