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
open BrowseT
open Browse_node

let section = Logger.Section.of_string "destruct"

exception Not_allowed of string
exception Useless_refine
exception Nothing_to_do

let () =
  Location.register_error_of_exn (function
    | Not_allowed s  -> Some (Location.error ("Destruct not allowed on " ^ s))
    | Useless_refine -> Some (Location.error "Cannot refine an useless branch")
    | Nothing_to_do  -> Some (Location.error "Nothing to do")
    | _ -> None
  )

let mk_id s  = Location.mknoloc (Longident.Lident s)
let mk_var s = Location.mknoloc s

module Predef_types = struct
  let char_ env ty =
    let a = Tast_helper.Pat.constant env ty (Asttypes.Const_char 'a') in
    let z = Parmatch.omega in
    [ a ; z ]

  let int_ env ty =
    let zero = Tast_helper.Pat.constant env ty (Asttypes.Const_int 0) in
    let n = Parmatch.omega in
    [ zero ; n ]

  let string_ env ty =
    let empty = Tast_helper.Pat.constant env ty (Ast_helper.const_string "") in
    let s = Parmatch.omega in
    [ empty ; s ]

  let tbl = Hashtbl.create 3

  let () =
    List.iter ~f:(fun (k, v) -> Hashtbl.add tbl k v) [
      Predef.path_char, char_ ;
      Predef.path_int, int_ ;
      Predef.path_string, string_ ;
    ]
end

let placeholder =
  let failwith = Ast_helper.Exp.ident (mk_id "failwith") in
  let todo = Ast_helper.Exp.constant (Ast_helper.const_string "TODO") in
  Ast_helper.Exp.apply failwith [ Raw_compat.Parsetree.arg_label_of_str "", todo ]

let shorten_path env path =
  Printtyp.shorten_path ~env path

let rec gen_patterns ?(recurse=true) env type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tvar _     -> raise (Not_allowed "non-immediate type")
  | Tarrow _   -> raise (Not_allowed "arrow type")
  | Tobject _  -> raise (Not_allowed "object type")
  | Tpackage _ -> raise (Not_allowed "modules")
  | Ttuple lst ->
    let patterns = Parmatch.omega_list lst in
    [ Tast_helper.Pat.tuple env type_expr patterns ]
  | Tconstr (path, _params, _) ->
    begin match Env.find_type_descrs path env with
    | [], [] ->
      if recurse then from_type_decl env path type_expr else
      raise (Not_allowed (sprintf "non-destructible type: %s" (Path.last path)))
    | [], labels ->
      let lst =
        List.map labels ~f:(fun lbl_descr ->
          let lidloc = mk_id lbl_descr.lbl_name in
          lidloc, lbl_descr,
          Tast_helper.Pat.var env type_expr (mk_var lbl_descr.lbl_name)
        )
      in
      [ Tast_helper.Pat.record env type_expr lst Asttypes.Closed ]
    | constructors, _ ->
      let prefix =
        let path = shorten_path env path in
        match Path.to_string_list path with
        | [] -> assert false
        | p :: ps ->
          fun name ->
            let open Longident in
            match
              List.fold_left ps ~init:(Lident p) ~f:(fun lid p -> Ldot (lid, p))
            with
            | Lident _ -> Lident name
            | Ldot (lid, _) -> Ldot (lid, name)
            | _ -> assert false
      in
      let are_types_unifiable typ =
        let snap = Btype.snapshot () in
        let res =
          try Ctype.unify_gadt ~newtype_level:0 (ref env) type_expr typ ; true
          with Ctype.Unify _trace -> false
        in
        Btype.backtrack snap ;
        res
      in
      List.filter_map constructors ~f:(fun cstr_descr ->
        if cstr_descr.cstr_generalized &&
           not (are_types_unifiable cstr_descr.cstr_res)
        then (
          Logger.debugf section (fun fmt name ->
            Format.fprintf fmt "Eliminating '%s' branch, its return type is not\
                               compatible with the expected type (%a)"
              name Printtyp.type_expr type_expr
          ) cstr_descr.cstr_name ;
          None
        ) else
          let args =
            if cstr_descr.cstr_arity <= 0 then [] else
              Parmatch.omegas cstr_descr.cstr_arity
          in
          let lidl = Location.mknoloc (prefix cstr_descr.cstr_name) in
          Some (Tast_helper.Pat.construct env type_expr lidl cstr_descr args)
      )
    end
  | Tvariant row_desc ->
    List.filter_map row_desc.row_fields ~f:(function
      | lbl, Rpresent param_opt ->
        let popt = Option.map param_opt ~f:(fun _ -> Parmatch.omega) in
        Some (Tast_helper.Pat.variant env type_expr lbl popt (ref row_desc))
      | _, _ -> None
    )
  | _ ->
    let fmt, to_string = Format.to_string () in
    Printtyp.type_expr fmt type_expr ;
    raise (Not_allowed (to_string ()))

and from_type_decl env path texpr =
  let tdecl = Env.find_type path env in
  match tdecl.Types.type_manifest with
  | Some te -> gen_patterns ~recurse:false env te
  | None ->
    try Hashtbl.find Predef_types.tbl path env texpr
    with Not_found ->
      raise (Not_allowed (sprintf "non-destructible type: %s" (Path.last path)))


let rec needs_parentheses = function
  | [] -> false
  | t :: ts ->
    match t with
    | Structure _
    | Structure_item _
    | Value_binding _ -> false
    | Expression e ->
      begin match e.Typedtree.exp_desc with
      | Typedtree.Texp_for _
      | Typedtree.Texp_while _ -> false
      | Typedtree.Texp_let _ ->
        (* We are after the "in" keyword, we need to look at the parent of the
           binding. *)
        needs_parentheses ts
      | Typedtree.Texp_function (_, cases, _) when List.length cases = 1 ->
        (* The assumption here is that we're not in a [function ... | ...]
            situation but either in [fun param] or [let name param]. *)
        needs_parentheses ts
      | _ -> true
      end
    | _ -> needs_parentheses ts

let rec get_every_pattern = function
  | [] -> assert false
  | parent :: parents ->
    match parent with
    | Case _
    | Pattern _ ->
      (* We are still in the same branch, going up. *)
      get_every_pattern parents
    | Expression e ->
      (* We are on the right node *)
      let patterns =
        Browse_node.fold_node (fun env loc node acc ->
          match node with
          | Pattern _ -> (* Not expected here *) assert false
          | Case _ ->
              Browse_node.fold_node (fun _env _loc node acc ->
                match node with
              | Pattern p -> p :: acc
              | _ -> acc
              ) env loc node acc
          | _ -> acc
        ) BrowseT.default_env BrowseT.default_loc parent []
      in
      let loc =
        Browse_node.fold_node (fun env loc node acc ->
          let open Location in
          if Lexing.compare_pos loc.loc_end acc.loc_end > 0 then loc else acc
        ) BrowseT.default_env (Browse.fix_loc parent) parent Location.none
      in
      loc, patterns
    | _ ->
      let j = Browse_misc.dump_ts [ BrowseT.of_node parent ] in
      let s = Json.to_string j in
      invalid_arg (sprintf "get_every_pattern: %s" s)(* Something went wrong. *)

let rec destructible patt =
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any | Tpat_var _ -> true
  | Tpat_alias (p, _, _)  -> destructible p
  | _ -> false

let is_package ty =
  match ty.Types.desc with
  | Types.Tpackage _ -> true
  | _ -> false

let node ~loc node parents =
  match node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let pexp = Untypeast.untype_expression expr in
    let needs_parentheses, result =
      if is_package ty then (
        let name = Location.mknoloc "M" in
        let mode = Ast_helper.Mod.unpack pexp in
        false, Ast_helper.Exp.letmodule name mode placeholder
      ) else (
        let ps = gen_patterns expr.Typedtree.exp_env ty in
        let cases  =
          List.map ps ~f:(fun patt ->
            let pc_lhs = Untypeast.untype_pattern patt in
            { Parsetree. pc_lhs ; pc_guard = None ; pc_rhs = placeholder }
          )
        in
        needs_parentheses parents, Ast_helper.Exp.match_ pexp cases
      )
    in
    let fmt, to_string = Format.to_string () in
    Pprintast.expression fmt result ;
    let str = to_string () in
    let str = if needs_parentheses then "(" ^ str ^ ")" else str in
    loc, str
  | Pattern patt ->
    let last_case_loc, patterns = get_every_pattern parents in
    List.iter patterns ~f:(fun p ->
      let p = Untypeast.untype_pattern p in
      Logger.infof section ~title:"EXISTING" Pprintast.pattern p
    ) ;
    let pss = List.map patterns ~f:(fun x -> [ x ]) in
    begin match Parmatch.complete_partial pss with
    | Some pat ->
      let pat  = Raw_compat.qualify_constructors shorten_path pat in
      let ppat = Untypeast.untype_pattern pat in
      let case = Ast_helper.Exp.case ppat placeholder in
      let loc =
        let open Location in
        { last_case_loc with loc_start = last_case_loc.loc_end }
      in
      let fmt, to_string = Format.to_string () in
      Pprintast.case_list fmt [ case ] ;
      loc, to_string ()
    | None ->
      if not (destructible patt) then raise Nothing_to_do else
      let ty = patt.Typedtree.pat_type in
      begin match gen_patterns patt.Typedtree.pat_env ty with
      | [] -> assert false (* we raise Not_allowed, but never return [] *)
      | [ more_precise ] ->
        (* If only one pattern is generated, then we're only refining the
           current pattern, not generating new branches. *)
        let ppat = Untypeast.untype_pattern more_precise in
        let fmt, to_string = Format.to_string () in
        Pprintast.pattern fmt ppat ;
        patt.Typedtree.pat_loc, to_string ()
      | sub_patterns ->
        let rev_before, after, top_patt =
          Raw_compat.find_branch patterns patt
        in
        let new_branches =
          List.map sub_patterns ~f:(fun by ->
            Raw_compat.subst_patt patt ~by top_patt
          )
        in
        let patterns =
          List.rev_append rev_before
            (List.append new_branches after)
        in
        let unused = Parmatch.return_unused patterns in
        let new_branches =
          List.fold_left unused ~init:new_branches ~f:(fun branches u ->
            match u with
            | `Unused p -> List.remove ~phys:true p branches
            | `Unused_subs (p, lst) ->
              List.map branches ~f:(fun branch ->
                if branch != p then branch else
                List.fold_left lst ~init:branch ~f:Raw_compat.rm_sub
              )
          )
        in
        match new_branches with
        | [] -> raise Useless_refine
        | p :: ps ->
          let p =
            List.fold_left ps ~init:p ~f:(fun acc p ->
              Tast_helper.Pat.pat_or top_patt.Typedtree.pat_env
                top_patt.Typedtree.pat_type acc p
            )
          in
          let ppat = Untypeast.untype_pattern p in
          let fmt, to_string = Format.to_string () in
          Pprintast.pattern fmt ppat ;
          top_patt.Typedtree.pat_loc, to_string ()
      end
    end
  | node ->
    raise (Not_allowed (string_of_node node))
