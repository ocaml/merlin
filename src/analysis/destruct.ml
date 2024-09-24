(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
open Browse_raw

exception Not_allowed of string
exception Useless_refine
exception Nothing_to_do
exception Ill_typed
exception Wrong_parent of string

let { Logger.log } = Logger.for_section "destruct"

let () =
  Location.register_error_of_exn (function
    | Not_allowed s -> Some (Location.error ("Destruct not allowed on " ^ s))
    | Useless_refine -> Some (Location.error "Cannot refine an useless branch")
    | Nothing_to_do -> Some (Location.error "Nothing to do")
    | Ill_typed ->
      Some (Location.error "The node on which destruct was called is ill-typed")
    | _ -> None)

let mk_id s = Location.mknoloc (Longident.Lident s)
let mk_var s = Location.mknoloc s

module Predef_types = struct
  let char_ env ty =
    let a = Tast_helper.Pat.constant env ty (Asttypes.Const_char 'a') in
    let z = Patterns.omega in
    [ a; z ]

  let int_ env ty =
    let zero = Tast_helper.Pat.constant env ty (Asttypes.Const_int 0) in
    let n = Patterns.omega in
    [ zero; n ]

  let string_ env ty =
    let empty =
      Tast_helper.Pat.constant env ty
        (Asttypes.Const_string ("", Location.none, None))
    in
    let s = Patterns.omega in
    [ empty; s ]

  let tbl = Hashtbl.create 3

  let () =
    List.iter
      ~f:(fun (k, v) -> Hashtbl.add tbl k v)
      [ (Predef.path_char, char_);
        (Predef.path_int, int_);
        (Predef.path_string, string_)
      ]
end

let placeholder = Ast_helper.Exp.hole ()

let rec gen_patterns ?(recurse = true) env type_expr =
  let open Types in
  log ~title:"gen_patterns" "%a" Logger.fmt (fun fmt ->
      Format.fprintf fmt "Generating patterns for type %a" Printtyp.type_expr
        type_expr);
  match get_desc type_expr with
  | Tlink _ -> assert false (* impossible after [Btype.repr] *)
  | Tvar _ -> raise (Not_allowed "non-immediate type")
  | Tarrow _ -> raise (Not_allowed "arrow type")
  | Tobject _ -> raise (Not_allowed "object type")
  | Tpackage _ -> raise (Not_allowed "modules")
  | Ttuple lst ->
    let patterns = Patterns.omega_list lst in
    [ Tast_helper.Pat.tuple env type_expr patterns ]
  | Tconstr (path, _params, _) -> begin
    match Env.find_type_descrs path env with
    | Type_record (labels, _) ->
      let lst =
        List.map labels ~f:(fun lbl_descr ->
            let lidloc = mk_id lbl_descr.lbl_name in
            ( lidloc,
              lbl_descr,
              Tast_helper.Pat.var Uid.internal_not_actually_unique env type_expr
                (mk_var lbl_descr.lbl_name) ))
      in
      [ Tast_helper.Pat.record env type_expr lst Asttypes.Closed ]
    | Type_variant (constructors, _) ->
      let prefix =
        let path = Printtyp.shorten_type_path env path in
        fun name ->
          let env_check = Env.find_constructor_by_name in
          Misc_utils.Path.to_shortest_lid ~env ~name ~env_check path
      in
      let are_types_unifiable typ =
        let snap = Btype.snapshot () in
        let res =
          try
            ignore
              (let pattern_env =
                 Ctype.Pattern_env.make env ~equations_scope:0
                   ~allow_recursive_equations:true
               in
               Ctype.unify_gadt pattern_env type_expr typ);
            true
          with Ctype.Unify _trace -> false
        in
        Btype.backtrack snap;
        res
      in
      List.filter_map constructors ~f:(fun cstr_descr ->
          if
            cstr_descr.cstr_generalized
            && not (are_types_unifiable cstr_descr.cstr_res)
          then (
            log ~title:"gen_patterns" "%a" Logger.fmt (fun fmt ->
                Format.fprintf fmt
                  "Eliminating '%s' branch, its return type is not compatible \
                   with the expected type (%a)"
                  cstr_descr.cstr_name Printtyp.type_expr type_expr);
            None)
          else
            let args =
              if cstr_descr.cstr_arity <= 0 then []
              else Patterns.omegas cstr_descr.cstr_arity
            in
            let lidl = Location.mknoloc (prefix cstr_descr.cstr_name) in
            Some
              (Tast_helper.Pat.construct env type_expr lidl cstr_descr args None))
    | _ ->
      if recurse then from_type_decl env path type_expr
      else
        raise
          (Not_allowed (sprintf "non-destructible type: %s" (Path.last path)))
  end
  | Tvariant row_desc ->
    List.filter_map (row_fields row_desc) ~f:(fun (lbl, row_field) ->
        match (lbl, row_field_repr row_field) with
        | lbl, Rpresent param_opt ->
          let popt = Option.map param_opt ~f:(fun _ -> Patterns.omega) in
          Some (Tast_helper.Pat.variant env type_expr lbl popt (ref row_desc))
        | _, Reither (_, l, _) ->
          let popt =
            match l with
            | [] -> None
            | _ :: _ -> Some Patterns.omega
          in
          Some (Tast_helper.Pat.variant env type_expr lbl popt (ref row_desc))
        | _, _ ->
          log ~title:"gen_patterns" "Absent";
          None)
  | _ ->
    let fmt, to_string = Format.to_string () in
    Printtyp.type_expr fmt type_expr;
    raise (Not_allowed (to_string ()))

and from_type_decl env path texpr =
  let tdecl = Env.find_type path env in
  match tdecl.Types.type_manifest with
  | Some te -> gen_patterns ~recurse:false env te
  | None -> (
    try Hashtbl.find Predef_types.tbl path env texpr
    with Not_found ->
      raise (Not_allowed (sprintf "non-destructible type: %s" (Path.last path)))
    )

let rec needs_parentheses = function
  | [] -> false
  | t :: ts -> (
    match t with
    | Structure _ | Structure_item _ | Value_binding _ -> false
    | Expression e -> begin
      match e.Typedtree.exp_desc with
      | Texp_for _ | Texp_while _ -> false
      | Texp_let _
      (* We are after the "in" keyword, we need to look at the parent of the
         binding. *)
      | Texp_function (_, Tfunction_body _)
      (* The assumption here is that we're not in a [function ... | ...]
          situation but either in [fun param] or [let name param]. *) ->
        needs_parentheses ts
      | _ -> true
    end
    | _ -> needs_parentheses ts)

let rec get_match = function
  | [] -> assert false
  | parent :: parents -> (
    match parent with
    | Case _ | Pattern _ ->
      (* We are still in the same branch, going up. *)
      get_match parents
    | Expression m -> (
      match m.Typedtree.exp_desc with
      | Typedtree.Texp_match (e, _, _) -> (m, e.exp_type)
      | Typedtree.Texp_function _ -> (
        let typ = m.exp_type in
        (* Function must have arrow type. This arrow type
           might be hidden behind type constructors *)
        ( m,
          match Types.get_desc typ with
          | Tarrow (_, te, _, _) -> te
          | Tconstr _ -> (
            match
              Ctype.full_expand ~may_forget_scope:true m.exp_env typ
              |> Types.get_desc
            with
            | Tarrow (_, te, _, _) -> te
            | _ -> assert false)
          | _ -> assert false ))
      | _ ->
        (* We were not in a match *)
        let s = Mbrowse.print_node () parent in
        raise (Not_allowed s))
    | _ ->
      (* We were not in a match *)
      let s = Mbrowse.print_node () parent in
      raise (Not_allowed s))

let collect_every_pattern_for_expression parent =
  let patterns =
    Mbrowse.fold_node
      (fun env node acc ->
        match node with
        | Pattern _ -> (* Not expected here *) raise Nothing_to_do
        | Case _ ->
          Mbrowse.fold_node
            (fun _env node acc ->
              match node with
              | Pattern p ->
                let ill_typed_pred =
                  Typedtree.
                    { f =
                        (fun p ->
                          List.memq Msupport.incorrect_attribute
                            ~set:p.pat_attributes)
                    }
                in
                if Typedtree.exists_general_pattern ill_typed_pred p then
                  raise Ill_typed
                else begin
                  match Typedtree.classify_pattern p with
                  | Value -> (p : Typedtree.pattern) :: acc
                  | Computation -> begin
                    match Typedtree.split_pattern p with
                    | Some p, _ -> (p : Typedtree.pattern) :: acc
                    | None, _ -> acc
                  end
                end
              | _ -> acc)
            env node acc
        | _ -> acc)
      Env.empty parent []
  in
  let loc =
    Mbrowse.fold_node
      (fun _ node acc ->
        let open Location in
        let loc = Mbrowse.node_loc node in
        if Lexing.compare_pos loc.loc_end acc.loc_end > 0 then loc else acc)
      Env.empty parent Location.none
  in
  (loc, patterns)

let collect_function_pattern loc param_pattern =
  match param_pattern.Typedtree.fp_kind with
  | Typedtree.Tparam_pat pattern -> (loc, [ pattern ])
  | Typedtree.Tparam_optional_default _ -> raise (Not_allowed "value_binding")

let rec get_every_pattern loc = function
  | [] -> assert false
  | parent :: parents -> (
    match parent with
    | Case _ | Pattern _ ->
      (* We are still in the same branch, going up. *)
      get_every_pattern loc parents
    | Expression { exp_desc = Typedtree.Texp_ident (Path.Pident id, _, _); _ }
      when Ident.name id = "*type-error*" -> raise Ill_typed
    | Expression { exp_desc = Typedtree.Texp_function (params, _body); _ } ->
    begin
      (* So we need to deal with the case where we're either in the body of a
         function, or in a function parameter. *)
      match
        List.find_some
          ~f:(fun param ->
            Location_aux.included ~into:param.Typedtree.fp_loc loc)
          params
      with
      | Some pattern ->
        (* In parameter case *)
        collect_function_pattern loc pattern
      | None ->
        (* In function body *)
        collect_every_pattern_for_expression parent
    end
    | Expression _ ->
      (* We are on the right node *)
      collect_every_pattern_for_expression parent
    | _ ->
      (* We were not in a match *)
      let s = Mbrowse.print_node () parent in
      raise (Not_allowed s))

let rec destructible patt =
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any | Tpat_var _ -> true
  | Tpat_alias (p, _, _, _) -> destructible p
  | _ -> false

let is_package ty =
  match ty.Types.desc with
  | Types.Tpackage _ -> true
  | _ -> false

let filter_attr =
  let default = Ast_mapper.default_mapper in
  let keep attr =
    let { Location.txt; _ }, _ = Ast_helper.Attr.as_tuple attr in
    not (String.is_prefixed ~by:"merlin." txt)
  in
  let attributes mapper attrs =
    default.Ast_mapper.attributes mapper (List.filter ~f:keep attrs)
  in
  { default with Ast_mapper.attributes }

let filter_expr_attr expr = filter_attr.Ast_mapper.expr filter_attr expr

let filter_pat_attr pat = filter_attr.Ast_mapper.pat filter_attr pat

let rec subst_patt initial ~by patt =
  let f = subst_patt initial ~by in
  if patt == initial then by
  else
    let open Typedtree in
    match patt.pat_desc with
    | Tpat_any | Tpat_var _ | Tpat_constant _ -> patt
    | Tpat_alias (p, x, y, uid) ->
      { patt with pat_desc = Tpat_alias (f p, x, y, uid) }
    | Tpat_tuple lst -> { patt with pat_desc = Tpat_tuple (List.map lst ~f) }
    | Tpat_construct (lid, cd, lst, lco) ->
      { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f, lco) }
    | Tpat_variant (lbl, pat_opt, row_desc) ->
      { patt with
        pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc)
      }
    | Tpat_record (sub, flg) ->
      let sub' =
        List.map sub ~f:(fun (lid, lbl_descr, patt) -> (lid, lbl_descr, f patt))
      in
      { patt with pat_desc = Tpat_record (sub', flg) }
    | Tpat_array lst -> { patt with pat_desc = Tpat_array (List.map lst ~f) }
    | Tpat_or (p1, p2, row) ->
      { patt with pat_desc = Tpat_or (f p1, f p2, row) }
    | Tpat_lazy p -> { patt with pat_desc = Tpat_lazy (f p) }

let rec rm_sub patt sub =
  let f p = rm_sub p sub in
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any | Tpat_var _ | Tpat_constant _ -> patt
  | Tpat_alias (p, x, y, uid) ->
    { patt with pat_desc = Tpat_alias (f p, x, y, uid) }
  | Tpat_tuple lst -> { patt with pat_desc = Tpat_tuple (List.map lst ~f) }
  | Tpat_construct (lid, cd, lst, lco) ->
    { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f, lco) }
  | Tpat_variant (lbl, pat_opt, row_desc) ->
    { patt with pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc) }
  | Tpat_record (sub, flg) ->
    let sub' =
      List.map sub ~f:(fun (lid, lbl_descr, patt) -> (lid, lbl_descr, f patt))
    in
    { patt with pat_desc = Tpat_record (sub', flg) }
  | Tpat_array lst -> { patt with pat_desc = Tpat_array (List.map lst ~f) }
  | Tpat_or (p1, p2, row) ->
    if p1 == sub then p2
    else if p2 == sub then p1
    else { patt with pat_desc = Tpat_or (f p1, f p2, row) }
  | Tpat_lazy p -> { patt with pat_desc = Tpat_lazy (f p) }

let rec qualify_constructors ~unmangling_tables f pat =
  let open Typedtree in
  let qualify_constructors = qualify_constructors ~unmangling_tables in
  let pat_desc =
    match pat.pat_desc with
    | Tpat_alias (p, id, loc, uid) ->
      Tpat_alias (qualify_constructors f p, id, loc, uid)
    | Tpat_tuple ps -> Tpat_tuple (List.map ps ~f:(qualify_constructors f))
    | Tpat_record (labels, closed) ->
      let labels =
        let open Longident in
        List.map labels ~f:(fun ((Location.{ txt; _ } as lid), lbl_des, pat) ->
            let lid_name = flatten txt |> String.concat ~sep:"." in
            let pat = qualify_constructors f pat in
            (* Un-mangle *)
            let _, labels = unmangling_tables in
            match Hashtbl.find_opt labels lid_name with
            | Some lbl_des ->
              ({ lid with txt = Lident lbl_des.Types.lbl_name }, lbl_des, pat)
            | None -> (lid, lbl_des, pat))
      in
      let closed =
        if List.length labels > 0 then
          let _, lbl_des, _ = List.hd labels in
          if List.length labels = Array.length lbl_des.Types.lbl_all then
            Asttypes.Closed
          else Asttypes.Open
        else closed
      in
      Tpat_record (labels, closed)
    | Tpat_construct (lid, cstr_desc, ps, lco) ->
      let lid =
        match lid.Asttypes.txt with
        | Longident.Lident name ->
          (* Un-mangle *)
          let name =
            let constrs, _ = unmangling_tables in
            match Hashtbl.find_opt constrs name with
            | Some cstr_des -> cstr_des.Types.cstr_name
            | None -> name
          in
          begin
            match Types.get_desc pat.pat_type with
            | Types.Tconstr (path, _, _) ->
              let path = f pat.pat_env path in
              let env_check = Env.find_constructor_by_name in
              let txt =
                Misc_utils.Path.to_shortest_lid ~env:pat.pat_env ~name
                  ~env_check path
              in
              { lid with Asttypes.txt }
            | _ -> lid
          end
        | _ -> lid (* already qualified *)
      in
      Tpat_construct
        (lid, cstr_desc, List.map ps ~f:(qualify_constructors f), lco)
    | Tpat_array ps -> Tpat_array (List.map ps ~f:(qualify_constructors f))
    | Tpat_or (p1, p2, row_desc) ->
      Tpat_or (qualify_constructors f p1, qualify_constructors f p2, row_desc)
    | Tpat_lazy p -> Tpat_lazy (qualify_constructors f p)
    | desc -> desc
  in
  { pat with pat_desc }

let find_branch patterns sub =
  let rec is_sub_patt patt ~sub =
    if patt == sub then true
    else
      let open Typedtree in
      match patt.pat_desc with
      | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_, None, _) ->
        false
      | Tpat_alias (p, _, _, _) | Tpat_variant (_, Some p, _) | Tpat_lazy p ->
        is_sub_patt p ~sub
      | Tpat_tuple lst | Tpat_construct (_, _, lst, _) | Tpat_array lst ->
        List.exists lst ~f:(is_sub_patt ~sub)
      | Tpat_record (subs, _) ->
        List.exists subs ~f:(fun (_, _, p) -> is_sub_patt p ~sub)
      | Tpat_or (p1, p2, _) -> is_sub_patt p1 ~sub || is_sub_patt p2 ~sub
  in
  let rec aux before = function
    | [] -> raise Nothing_to_do
    | p :: after when is_sub_patt p ~sub -> (before, after, p)
    | p :: ps -> aux (p :: before) ps
  in
  aux [] patterns

(* In the presence of record punning fields, the definition must be
   reconstructed with the label. ie: [{a; b}] with destruction on [a]
   becomes [{a = destruct_result; b}]. *)
let find_field_name_for_punned_field patt = function
  | Pattern { pat_desc = Tpat_record (fields, _); _ } :: _ ->
    List.find_opt
      ~f:(fun (_, _, opat) ->
        let ppat_loc = patt.Typedtree.pat_loc
        and opat_loc = opat.Typedtree.pat_loc in
        Int.equal (Location_aux.compare ppat_loc opat_loc) 0)
      fields
    |> Option.map ~f:(fun (_, label, _) -> label)
  | _ -> None

let print_pretty ?punned_field config source subject =
  let result = Mreader.print_pretty config source subject in
  match punned_field with
  | None -> result
  | Some label -> label.Types.lbl_name ^ " = " ^ result

(* conversion from Typedtree.pattern to Parsetree.pattern list *)
module Conv = struct
  open Asttypes
  open Types
  open Typedtree
  open Parsetree
  let mkpat desc = Ast_helper.Pat.mk desc

  let name_counter = ref 0
  let fresh name =
    let current = !name_counter in
    name_counter := !name_counter + 1;
    "#$" ^ name ^ Int.to_string current

  let conv typed =
    let constrs = Hashtbl.create 7 in
    let labels = Hashtbl.create 7 in
    let rec loop pat =
      match pat.pat_desc with
      | Tpat_or (pa, pb, _) -> mkpat (Ppat_or (loop pa, loop pb))
      | Tpat_var (_, ({ txt = "*extension*"; _ } as nm), _) ->
        (* PR#7330 *)
        mkpat (Ppat_var nm)
      | Tpat_any | Tpat_var _ -> mkpat Ppat_any
      | Tpat_constant c -> mkpat (Ppat_constant (Untypeast.constant c))
      | Tpat_alias (p, _, _, _) -> loop p
      | Tpat_tuple lst -> mkpat (Ppat_tuple (List.map ~f:loop lst))
      | Tpat_construct (cstr_lid, cstr, lst, _) ->
        let id = fresh cstr.cstr_name in
        let lid = { cstr_lid with txt = Longident.Lident id } in
        Hashtbl.add constrs id cstr;
        let arg =
          match List.map ~f:loop lst with
          | [] -> None
          | [ p ] -> Some ([], p)
          | lst -> Some ([], mkpat (Ppat_tuple lst))
        in
        mkpat (Ppat_construct (lid, arg))
      | Tpat_variant (label, p_opt, _row_desc) ->
        let arg = Option.map ~f:loop p_opt in
        mkpat (Ppat_variant (label, arg))
      | Tpat_record (subpatterns, _closed_flag) ->
        let fields =
          List.map
            ~f:(fun (_, lbl, p) ->
              let id = fresh lbl.lbl_name in
              Hashtbl.add labels id lbl;
              (mknoloc (Longident.Lident id), loop p))
            subpatterns
        in
        mkpat (Ppat_record (fields, Open))
      | Tpat_array lst -> mkpat (Ppat_array (List.map ~f:loop lst))
      | Tpat_lazy p -> mkpat (Ppat_lazy (loop p))
    in
    let ps = loop typed in
    (ps, constrs, labels)
end

let need_recover_labeled_args = function
  | Parsetree.Pexp_construct ({ loc; txt = Longident.Lident ctor }, Some e) ->
    (* If the internal construction is ghosted, then the expression must be
       re-labelled. *)
    if String.equal "Some" ctor && loc.loc_ghost then Some e else None
  | _ -> None

let remove_non_applied_optional_args (Parsetree.{ pexp_desc; _ } as base_expr) =
  (* Fix the behaviour described here
     https://github.com/ocaml/merlin/issues/1770 *)
  match pexp_desc with
  | Parsetree.Pexp_apply (expr, args) ->
    let args =
      List.concat_map
        ~f:(fun (label, (expr : Parsetree.expression)) ->
          match (label, expr.pexp_loc.loc_ghost, expr.pexp_desc) with
          | ( Asttypes.Optional _,
              true,
              Pexp_construct ({ txt = Longident.Lident "None"; _ }, _) ) -> []
          | Asttypes.Optional str, false, exp_desc -> (
            match need_recover_labeled_args exp_desc with
            | Some e -> [ (Asttypes.Labelled str, e) ]
            | None -> [ (label, expr) ])
          | _ -> [ (label, expr) ])
        args
    in
    let pexp_desc = Parsetree.Pexp_apply (expr, args) in
    { base_expr with pexp_desc }
  | _ -> base_expr

let destruct_expression loc config source parents expr =
  let ty = expr.Typedtree.exp_type in
  let pexp =
    filter_expr_attr (Untypeast.untype_expression expr)
    |> remove_non_applied_optional_args
  in
  let () =
    log ~title:"node_expression" "%a" Logger.fmt (fun fmt ->
        Printast.expression 0 fmt pexp)
  in
  let needs_parentheses, result =
    if is_package (Types.Transient_expr.repr ty) then
      let mode = Ast_helper.Mod.unpack pexp in
      (false, Ast_helper.Exp.letmodule_no_opt "M" mode placeholder)
    else
      let ps = gen_patterns expr.Typedtree.exp_env ty in
      let cases =
        List.map ps ~f:(fun patt ->
            let pc_lhs = filter_pat_attr (Untypeast.untype_pattern patt) in
            { Parsetree.pc_lhs; pc_guard = None; pc_rhs = placeholder })
      in
      (needs_parentheses parents, Ast_helper.Exp.match_ pexp cases)
  in
  let str = Mreader.print_pretty config source (Pretty_expression result) in
  let str = if needs_parentheses then "(" ^ str ^ ")" else str in
  (loc, str)

let refine_partial_match last_case_loc config source patterns =
  let cases =
    List.map patterns ~f:(fun pat ->
        let _pat, constrs, labels = Conv.conv pat in
        let unmangling_tables = (constrs, labels) in
        (* Unmangling and prefixing *)
        let pat =
          qualify_constructors ~unmangling_tables Printtyp.shorten_type_path pat
        in
        (* Untyping and casing *)
        let ppat = filter_pat_attr (Untypeast.untype_pattern pat) in
        Ast_helper.Exp.case ppat placeholder)
  in
  let loc = Location.{ last_case_loc with loc_start = last_case_loc.loc_end } in
  let str = Mreader.print_pretty config source (Pretty_case_list cases) in
  (loc, str)

let filter_new_branches new_branches patterns =
  let unused = Parmatch.return_unused patterns in
  List.fold_left unused ~init:new_branches ~f:(fun branches u ->
      match u with
      | `Unused p -> List.remove ~phys:true p branches
      | `Unused_subs (p, lst) ->
        List.map branches ~f:(fun branch ->
            if branch != p then branch
            else List.fold_left lst ~init:branch ~f:rm_sub))

let refine_current_pattern parents patt config source generated_pattern =
  let punned_field = find_field_name_for_punned_field patt parents in
  let ppat = filter_pat_attr (Untypeast.untype_pattern generated_pattern) in
  let str = print_pretty ?punned_field config source (Pretty_pattern ppat) in
  (patt.Typedtree.pat_loc, str)

let refine_and_generate_branches patt config source patterns sub_patterns =
  let rev_before, after, top_patt = find_branch patterns patt in
  let new_branches =
    List.map sub_patterns ~f:(fun by -> subst_patt patt ~by top_patt)
  in
  let patterns = after @ rev_before @ new_branches in
  match filter_new_branches new_branches patterns with
  | [] -> raise Useless_refine
  | p :: ps ->
    let p =
      List.fold_left ps ~init:p ~f:(fun acc p ->
          Tast_helper.Pat.pat_or top_patt.Typedtree.pat_env
            top_patt.Typedtree.pat_type acc p)
    in
    (* Format.eprintf "por %a \n%!" (Printtyped.pattern 0) p; *)
    let ppat = filter_pat_attr (Untypeast.untype_pattern p) in
    (* Format.eprintf "ppor %a \n%!" (Pprintast.pattern) ppat; *)
    let str = Mreader.print_pretty config source (Pretty_pattern ppat) in
    (* Format.eprintf "STR: %s \n %!" str; *)
    (top_patt.Typedtree.pat_loc, str)

let refine_complete_match (type a) parents (patt : a Typedtree.general_pattern)
    config source patterns =
  match Typedtree.classify_pattern patt with
  | Computation -> raise (Not_allowed "computation pattern")
  | Value ->
    let _ : Typedtree.value Typedtree.general_pattern = patt in
    if not (destructible patt) then raise Nothing_to_do
    else
      let ty = patt.Typedtree.pat_type in
      begin
        match gen_patterns patt.Typedtree.pat_env ty with
        | [] -> assert false
        | [ more_precise_pattern ] ->
          (* If only one pattern is generated, then we're only refining the
             current pattern, not generating new branches. *)
          refine_current_pattern parents patt config source more_precise_pattern
        | sub_patterns ->
          (* If more than one pattern is generated, then we're generating new
             branches. *)
          refine_and_generate_branches patt config source patterns sub_patterns
      end

let destruct_pattern (type a) (patt : a Typedtree.general_pattern) config source
    loc parents =
  let last_case_loc, patterns = get_every_pattern loc parents in
  (* Printf.eprintf "tot %d o%!"(List.length patterns); *)
  let () =
    List.iter patterns ~f:(fun p ->
        let p = filter_pat_attr (Untypeast.untype_pattern p) in
        log ~title:"EXISTING" "%t" (fun () ->
            Mreader.print_pretty config source (Pretty_pattern p)))
  in
  let pss = List.map patterns ~f:(fun x -> [ x ]) in
  let m, e_typ = get_match parents in
  let pred =
    Typecore.partial_pred ~lev:Btype.generic_level m.Typedtree.exp_env e_typ
  in
  match Parmatch.complete_partial ~pred pss with
  | [] ->
    (* The match is already complete, we try to refine it *)
    refine_complete_match parents patt config source patterns
  | patterns -> refine_partial_match last_case_loc config source patterns

let rec destruct_record config source selected_node = function
  | (Expression { exp_desc = Texp_field _; _ } as parent) :: rest ->
    node config source parent rest
  | Expression e :: rest -> node config source (Expression e) rest
  | _ -> raise (Not_allowed (string_of_node selected_node))

and node config source selected_node parents =
  let loc = Mbrowse.node_loc selected_node in
  match selected_node with
  | Record_field (`Expression _, _, _) ->
    destruct_record config source selected_node parents
  | Expression expr -> destruct_expression loc config source parents expr
  | Pattern patt -> destruct_pattern patt config source loc parents
  | node -> raise (Not_allowed (string_of_node node))
