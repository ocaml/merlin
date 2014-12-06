open Std
open BrowseT
       
let section = Logger.Section.of_string "destruct"

exception Not_allowed of string
exception Nothing_to_do

let () =
  Location.register_error_of_exn (function
    | Not_allowed s -> Some (Location.error ("Destruct not allowed on " ^ s))
    | Nothing_to_do -> Some (Location.error "Nothing to do")
    | _ -> None
  )

let assert_false =
  let _false = Location.mknoloc (Longident.Lident "false") in
  Ast_helper.Exp.assert_ (Ast_helper.Exp.construct _false None)

module Tast_helper = (* TODO: move in dedicated module inside ocaml_XXX/ *)
struct
  open Typedtree

  module Pat = struct
    let pat_extra = []
    let pat_attributes = []

    let var ?loc pat_env pat_type str =
      let pat_loc =
        match loc with
        | None -> str.Asttypes.loc
        | Some loc -> loc
      in
      let pat_desc = Tpat_var (Ident.create str.Asttypes.txt, str) in
      { pat_desc; pat_loc; pat_extra; pat_attributes; pat_type; pat_env }

    let record ?(loc=Location.none) pat_env pat_type lst closed_flag =
      let pat_desc = Tpat_record (lst, closed_flag) in
      { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

    let tuple ?(loc=Location.none) pat_env pat_type lst =
      let pat_desc = Tpat_tuple lst in
      { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

    let construct ?(loc=Location.none) pat_env pat_type lid cstr_desc args =
      let pat_desc = Tpat_construct (lid, cstr_desc, args) in
      { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }

    let pat_or ?(loc=Location.none) ?row_desc pat_env pat_type p1 p2 =
      let pat_desc = Tpat_or (p1, p2, row_desc) in
      { pat_desc; pat_loc = loc; pat_extra; pat_attributes; pat_type; pat_env }
  end
end

let gen_patterns env type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tvar _ -> raise (Not_allowed "non-immediate type")
  | Tarrow _ -> raise (Not_allowed "arrow type")
  | Ttuple lst ->
    let patterns = Parmatch.omega_list lst in
    [ Tast_helper.Pat.tuple env type_expr patterns ]
  | Tconstr (path, _params, _) ->
    begin match Env.find_type_descrs path env with
    | [], [] ->
      raise (Not_allowed (sprintf "non-destructible type: %s" @@ Path.last path))
    | [], labels ->
      let lst =
        List.map labels ~f:(fun lbl_descr ->
          let lidloc = Location.mknoloc (Longident.Lident lbl_descr.lbl_name) in
          lidloc, lbl_descr,
          Tast_helper.Pat.var env type_expr (Location.mknoloc lbl_descr.lbl_name)
        )
      in
      [ Tast_helper.Pat.record env type_expr lst Asttypes.Closed ]
    | constructors, _ ->
      List.map constructors ~f:(fun cstr_descr ->
        let args =
          if cstr_descr.cstr_arity <= 0 then [] else
          Parmatch.omegas cstr_descr.cstr_arity
        in
        let lidl = Location.mknoloc (Longident.Lident cstr_descr.cstr_name) in
        Tast_helper.Pat.construct env type_expr lidl cstr_descr args
      )
    end
  | Tvariant row_desc ->
    (* TODO: use [row_name]? *)
    failwith "TODO(polymorphic variants)"
  | _ ->
    failwith "TODO(get_patterns)"

let rec needs_parentheses = function
  | [] -> false
  | t :: ts ->
    match t.t_node with
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
    match parent.t_node with
    | Case _
    | Pattern _ ->
      (* We are still in the same branch, going up. *)
      get_every_pattern parents
    | Expression e ->
      (* We are on the right node *)
      e, List.concat_map (Lazy.force parent.t_children) ~f:(fun c ->
        match c.t_node with
        | Pattern _ -> (* Not expected here *) assert false
        | Case _ ->
          List.filter_map (Lazy.force c.t_children) ~f:(fun patt ->
            match patt.t_node with
            | Pattern p ->
              Logger.debugf section ~title:"GET EVERY PAT" Location.print_loc p.Typedtree.pat_loc ;
              Some p
            | _ -> None
          )
        | _ -> []
      )
    | _ ->
      let j = Browse_misc.dump_ts [ parent ] in
      let s = Json.to_string j in
      invalid_arg (sprintf "get_every_pattern: %s" s)(* Something went wrong. *)

let insert_pattern expr pat =
  let pexpr  = Untypeast.untype_expression expr in
  let pc_lhs = Untypeast.untype_pattern pat in
  let open Parsetree in
  match pexpr.pexp_desc with
  | Pexp_match (expr, cases) ->
    let case = { pc_lhs ; pc_guard = None ; pc_rhs = assert_false } in
    { pexpr with pexp_desc = Pexp_match (expr, cases @ [ case ]) }
  | _ ->
    assert false

let rec destructible patt =
  let open Typedtree in
  match patt.pat_desc with
  | Tpat_any | Tpat_var _ -> true
  | Tpat_alias (p, _, _)  -> destructible p
  | _ -> false

let subst_patt initial ~by patt =
  let changed = ref false in
  let rec f patt =
    let open Typedtree in
    if patt == initial then ( changed := true ; by ) else
    match patt.pat_desc with
    | Tpat_any
    | Tpat_var _
    | Tpat_constant _ -> patt
    | Tpat_alias (p,x,y) ->
      { patt with pat_desc = Tpat_alias (f p, x, y) }
    | Tpat_tuple lst ->
      { patt with pat_desc = Tpat_tuple (List.map lst ~f)}
    | Tpat_construct (lid, cd, lst) ->
      { patt with pat_desc = Tpat_construct (lid, cd, List.map lst ~f) }
    | Tpat_variant (lbl, pat_opt, row_desc) ->
      { patt with pat_desc = Tpat_variant (lbl, Option.map pat_opt ~f, row_desc) }
    | Tpat_record (sub, flg) ->
      let sub' =
        List.map sub ~f:(fun (lid, lbl_descr, patt) -> lid, lbl_descr, f patt)
      in
      { patt with pat_desc = Tpat_record (sub', flg) }
    | Tpat_array lst ->
      { patt with pat_desc = Tpat_array (List.map lst ~f)}
    | Tpat_or (p1, p2, row) ->
      { patt with pat_desc = Tpat_or (f p1, f p2, row) }
    | Tpat_lazy p ->
      { patt with pat_desc = Tpat_lazy (f p) }
  in
  let patt' = f patt in
  !changed, patt'

let node ~loc ~env parents node =
  match node.t_node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let ps = gen_patterns env ty in
    let cases  =
      List.map ps ~f:(fun patt ->
        let pc_lhs = Untypeast.untype_pattern patt in
        Parsetree.{ pc_lhs ; pc_guard = None ; pc_rhs = assert_false }
      )
    in
    let pexp   = Untypeast.untype_expression expr in
    let match_ = Ast_helper.Exp.match_ pexp cases in
    let fmt, to_string = Format.to_string () in
    Pprintast.expression fmt match_ ;
    let str = to_string () in
    let str = if needs_parentheses parents then "(" ^ str ^ ")" else str in
    loc, str
  | Pattern patt ->
    let expr, patterns = get_every_pattern parents in
    List.iter patterns ~f:(fun p ->
      let p = Untypeast.untype_pattern p in
      Logger.infof section ~title:"EXISTING" Pprintast.pattern p
    ) ;
    let pss = List.map patterns ~f:(fun x -> [ x ]) in
    begin match Parmatch.complete_partial pss with
    | Some p ->
      let pexpr = insert_pattern expr p in
      let fmt, to_string = Format.to_string () in
      Pprintast.expression fmt pexpr ;
      expr.Typedtree.exp_loc, to_string ()
    | None ->
      Logger.info section "Total matching" ;
      if not (destructible patt) then raise Nothing_to_do else
      let ty = patt.Typedtree.pat_type in
      begin match gen_patterns env ty with
      | [] -> assert false (* we raise Not_allowed, but never return [] *)
      | [ more_precise ] ->
        (* If only one pattern is generated, then we're only refining the
           current pattern, not generating new branches. *)
        let ppat = Untypeast.untype_pattern more_precise in
        let fmt, to_string = Format.to_string () in
        Pprintast.pattern fmt ppat ;
        patt.Typedtree.pat_loc, to_string ()
      | replacement :: _ ->
        let loc = ref patt.Typedtree.pat_loc in
        let rep = ref replacement in
        let patterns =
          List.map patterns ~f:(fun p ->
            let changed, p' = subst_patt patt ~by:replacement p in
            if changed then (
              loc := p.Typedtree.pat_loc ;
              rep := p'
            ) ;
            p'
          )
        in
        let pss = List.map patterns ~f:(fun x -> [ x ]) in
        match Parmatch.complete_partial pss with
        | None ->
          (* [get_patterns] generated more than one, so we *must* have something
             here. *)
          assert false 
        | Some p ->
          let p = Tast_helper.Pat.pat_or env p.Typedtree.pat_type !rep p in
          let ppat = Untypeast.untype_pattern p in
          let fmt, to_string = Format.to_string () in
          Pprintast.pattern fmt ppat ;
          !loc, to_string ()
      end
    end
  | _ ->
    failwith "not handled"
