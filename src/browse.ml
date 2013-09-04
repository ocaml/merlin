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

open Std
open Typedtree

type mod_info =
  | Named of string
  | Include of Ident.t list
  | Alias of Path.t
  | Structure

type context =
  | Expr      of Types.type_expr
  | Pattern   of Ident.t option * Types.type_expr
  | Type      of Types.type_expr
  | TypeDecl  of Ident.t * Types.type_declaration
  | Module    of mod_info * Types.module_type
  | Modtype   of Ident.t * Types.modtype_declaration
  | Class     of Ident.t * Types.class_declaration
  | ClassType of Ident.t * Types.class_type_declaration
  | MethodCall of Types.type_expr * string
  | NamedOther of Ident.t
  | TopStructure
  | Other

(* Typedtree navigation made easy *)
type t = {
  loc : Location.t;
  env : Env.t;
  context : context;
  nodes : t list Lazy.t
}

let dummy = { loc = Location.none ; env = Env.empty ;
              context = Other; nodes = lazy [] }

let singleton ?(context=Other) ?(nodes=lazy []) loc env = 
  { loc ; env ; context ; nodes }

let rec structure { str_final_env ; str_items } =
  List.map str_items ~f:(structure_item ~env:str_final_env)

and structure_item ~env { str_desc ; str_loc ; str_env } =
  { loc = str_loc ; env = str_env ; context = TopStructure ;
    nodes = lazy (structure_item_desc ~env str_desc) }

and structure_item_desc ~env = function
  | Tstr_eval e            -> [expression e]
  | Tstr_value (_,pes)     -> patterns ~env pes
  | Tstr_primitive (i,l,_)
  | Tstr_modtype (i,l,_) (* TODO: change to Modtype *)
  | Tstr_exception (i,l,_)
  | Tstr_exn_rebind (i,l,_,_) ->
    [singleton ~context:(NamedOther i) l.Location.loc env]
  | Tstr_module (_,s,m) -> [module_binding s m]
  | Tstr_recmodule ms ->
    List.map (fun (i,s,_,m) -> module_binding s m) ms
  | Tstr_type ilds ->
    let aux (id,_,ty) = type_declaration ~env id ty in
    List.map ~f:aux ilds
  | Tstr_open _               -> []
  | Tstr_class lst            -> List.map ~f:(class_declaration ~env) lst
  | Tstr_class_type lst ->
    List.map lst ~f:(fun (id,l,{ ci_type_decl }) ->
      singleton ~context:(ClassType (id, ci_type_decl)) l.Location.loc env
    )
  | Tstr_include (m,ids) -> [module_include ids m]

and type_declaration ~env id { typ_loc ; typ_type ; typ_manifest } =
  let nodes = Option.map typ_manifest ~f:(fun c -> lazy [core_type c]) in
  singleton 
    ~context:(TypeDecl (id,typ_type))
    ?nodes
    typ_loc env

and core_type { ctyp_env ; ctyp_loc ; ctyp_desc ; ctyp_type } =
  let subtypes = match ctyp_desc with
    | Ttyp_any | Ttyp_var _ -> []
    | Ttyp_arrow (_,t1,t2) -> [t1;t2]
    | Ttyp_tuple ts | Ttyp_constr (_,_,ts) | Ttyp_class (_,_,ts,_) -> ts
    | Ttyp_alias (t,_) | Ttyp_poly (_,t) -> [t]
    | Ttyp_package _ | Ttyp_object _ | Ttyp_variant _ ->
      (*FIXME: case-by-case*) []
  in
  singleton 
    ~context:(Type ctyp_type)
    ~nodes:(lazy (List.map ~f:core_type subtypes))
             ctyp_loc ctyp_env

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
  let fields = List.filter_map class_struct.cstr_fields ~f:(class_field ~env) in
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

and patterns ?env pes =
  List.fold_left pes ~init:[] ~f:(fun ls (p,e) ->
    let l = pattern ?env p in
    l :: expression e :: ls
  )

and pattern ?env { pat_loc ; pat_type ; pat_desc ; pat_env } =
  let subpatterns = match pat_desc with
    | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> []
    | Tpat_alias (p,_,_) | Tpat_lazy p | Tpat_variant (_,Some p,_) -> [p]
    | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,_,ps,_) -> ps
    | Tpat_or (p1,p2,_) -> [p1;p2]
    | Tpat_record (r,_) -> List.map ~f:(fun (_,_,_,p) -> p) r
  in
  let name =
    match pat_desc with
    | Tpat_var (i, _) -> Some i
    | _ -> None
  in
  singleton 
    ~context:(Pattern (name, pat_type))
    ~nodes:(lazy (List.map ~f:(pattern ?env) subpatterns)) pat_loc 
    (match env with Some e' -> e' | _ -> pat_env)

and expression_extra ~env t = function
  | Texp_open (_,_,env),loc -> { loc ; env ; context = Other ; nodes = lazy [t] }
  | Texp_constraint (c1,c2), loc ->
    let cs = match c1,c2 with
      | Some c1, Some c2 -> [c1;c2] | Some c, _ | _, Some c -> [c] | _ -> []
    in
    { loc ; env ; context = Other ; nodes = lazy (t :: List.map ~f:core_type cs) } 
  | Texp_poly (Some c), loc ->
    { loc ; env ; context = Other ; nodes = lazy [core_type c ; t] } 
  | _ -> t

and expression { exp_desc ; exp_loc ; exp_extra ; exp_type ; exp_env } =
  let expression_desc = function
    | Texp_ident (_,_,_) -> []
    | Texp_constant _ -> []
    | Texp_let (_,pes,e) -> expression e :: patterns ~env:e.exp_env pes
    | Texp_function (_,pes,_) -> patterns pes
    | Texp_apply (e,leso) ->
      let f = function (_,Some e,_) -> Some (expression e) | _ -> None in
      expression e :: List.filter_map leso ~f
    | Texp_match (e,pes,_) -> expression e :: patterns pes
    | Texp_try (e,pes) -> expression e :: patterns pes
    | Texp_tuple (es) -> List.map ~f:expression es
    | Texp_construct (_,_,_,es,_) -> List.map ~f:expression es
    | Texp_variant (_,Some e) -> [expression e]
    | Texp_variant (_,None) -> []
    | Texp_record (pldes,Some e) ->
      expression e :: List.map ~f:(fun (_,_,_,e) -> expression e) pldes
    | Texp_record (pldes,None) -> List.map ~f:(fun (_,_,_,e) -> expression e) pldes
    | Texp_array es -> List.map ~f:expression es
    | Texp_send (ea, m, eb') ->
      let tail = match eb' with None -> [] | Some eb -> [expression eb] in
      let m = Location.(meth ea m ea.exp_loc.loc_end exp_loc.loc_end) in
      expression ea :: m :: tail
    | Texp_assert ea
    | Texp_lazy ea
    | Texp_setinstvar (_,_,_,ea)
    | Texp_field (ea,_,_,_) -> [expression ea]
    | Texp_ifthenelse (ea,eb,None)
    | Texp_setfield (ea,_,_,_,eb)
    | Texp_sequence (ea,eb)
    | Texp_when (ea,eb)
    | Texp_while (ea,eb) -> [expression ea ; expression eb]
    | Texp_for (_,_,ea,eb,_,ec)
    | Texp_ifthenelse (ea,eb,Some ec) -> List.map ~f:expression [ea;eb;ec]
    | Texp_override (_,ples) -> List.map ~f:(fun (_,_,e) -> expression e) ples
    | Texp_letmodule (_,s,m,e) -> (expression e) :: [module_binding s m]
    | Texp_assertfalse -> []
    | Texp_pack m -> [module_expr m]
    | Texp_object (cls,_) -> class_structure ~env:exp_env cls
    | Texp_new _
    | Texp_instvar _ -> [] (*FIXME*)
  in
  List.fold_left exp_extra ~f:(expression_extra ~env:exp_env) ~init:{
    loc = exp_loc ;
    env = exp_env ;
    context = Expr exp_type ;
    nodes = lazy (expression_desc exp_desc)
  }

and module_include ids ({ mod_env ; mod_desc ; mod_type ; mod_loc } as def) =
  let mod_info =
    match mod_desc with
    | Tmod_ident (p, _loc) -> Some (Alias p)
    | _ -> None
  in
  {
    loc = mod_loc ;
    env = mod_env ;
    context = Module (Include ids, mod_type) ;
    nodes = lazy [module_expr ?mod_info def] ;
  }

and module_binding name ({ mod_env ; mod_desc ; mod_type ; mod_loc } as def) =
  let mod_info =
    match mod_desc with
    | Tmod_ident (p, _loc) -> Some (Alias p)
    | _ -> None
  in
  {
    loc = name.Location.loc ;
    env = mod_env ;
    context = Module (Named name.Location.txt, mod_type) ;
    nodes = lazy [module_expr ?mod_info def] ;
  }

and module_expr ?(mod_info=Structure) { mod_env ; mod_desc ; mod_type ; mod_loc } =
  { loc = mod_loc ; env = mod_env ; context = Module (mod_info, mod_type) ;
    nodes = lazy (module_expr_desc mod_desc) }

and module_expr_desc = function
  | Tmod_ident _ -> []
  | Tmod_structure s -> structure s
  | Tmod_constraint (e,_,_,_) -> [module_expr e]
  (* TODO: use name *)
  | Tmod_functor (_,s,_,e) -> [module_binding s e]
  | Tmod_apply (e1,e2,_) -> [module_expr e1 ; module_expr e2]
  | Tmod_unpack (e,_) -> [expression e]

and meth obj name loc_start loc_end =
  let name = match name with
    | Typedtree.Tmeth_name s -> s
    | Typedtree.Tmeth_val i -> Ident.name i
  in
  singleton ~context:(MethodCall (obj.exp_type,name))
    { Location. loc_start ; loc_end ; loc_ghost = false }
    obj.exp_env

let local_near pos nodes =
  let cmp = Location.compare_pos pos in
  let best_of ({ loc = l1 } as t1) ({ loc = l2 } as t2) =
    match cmp l1, cmp l2 with
    | 0, 0 ->
      (* Cursor is inside locations: select larger one... not sure why :-) *)
      if Location.(Misc.compare_pos l1.loc_end l2.loc_end) < 0
      then t2
      else t1
      (* Cursor inside one location, prefer it *)
    | 0, _ -> t1
    | _, 0 -> t2
    | _, _ ->
      (* Cursor outside locations, select the rightmost one *)
      if Location.(Misc.compare_pos l1.loc_end l2.loc_end) < 0
      then t2
      else t1
  in
  List.fold_left nodes ~init:None ~f:(fun best t ->
    match cmp t.loc, best with
    | n, _ when n < 0 -> best
    | n, None -> Some t
    | n, Some t' -> Some (best_of t t')
  )

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
  Option.map (local_near pos envs) ~f:(fun t -> List.hd (traverse_branch pos t))

let nearest_before pos envs =
  Option.bind (local_near pos envs) ~f:(fun t ->
    let branch = traverse_branch pos t in
    let rec aux = function
      | a :: b :: tail when is_enclosing pos b -> Some a
      (* No node matched: fallback to deepest before behavior *)
      | [x] -> Some (List.hd branch)
      | [] -> None
      | _ :: tail -> aux tail
    in
    aux branch
  )

let enclosing pos envs =
  let not_enclosing l = not (is_enclosing pos l) in
  match local_near pos envs with
  | None -> []
  | Some t ->
    let results = traverse_branch pos t in
    Misc.list_drop_while not_enclosing results
