(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi *)
(*
   Match a pattern against a program
*)

open Asttypes
open Parsetree
open Typedtree
open Longident

exception Cannot_parse_type of exn

(* private exception used to fail a match *)
exception DontMatch

(* Equivalent of [Compmisc.initial_env ()] from upstream compiler-libs.
   Inlined here to avoid depending on [merlin_specific] for one helper. *)
let initial_env = lazy (
  let initially_opened_module =
    if !Clflags.nopervasives then None else Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~initially_opened_module
    ~open_implicit_modules:(List.rev !Clflags.open_modules)
)

let parse_type t =
  let env = Lazy.force initial_env in
  try (Typetexp.transl_type_scheme env t).ctyp_type
  with e -> raise (Cannot_parse_type e)

let memoize h f k =
  match Hashtbl.find_opt h k with
  | None -> let r = f k in Hashtbl.add h k r; r
  | Some r -> r

(* warning: global, ever-growing cache *)
let parse_type = memoize (Hashtbl.create 10) parse_type

(* This global is cleared before each search.
   Consider passing it around explicitly as part of an 'env' argument. *)
let wildcards = ref ([] : (Asttypes.label * Parsetree.expression) list)

(* wildcards are in the form __123 ie.
   the two first characters are underscores;
   the following characters are digits.
*)
let is_wildcard str =
  String.length str > 2 && str.[0] = '_' && str.[1] = '_' &&
  let r = ref true in
  for i = 2 to String.length str - 1 do
    match str.[i] with
    | '0'..'9' -> ()
    | _ -> r := false;
  done;
  !r

let check_wildcard id e =
  try
    let e' = List.assoc id !wildcards in
    if e <> e' then raise DontMatch
  with Not_found -> wildcards := (id, e) :: !wildcards

let check_wildcard_lid id lid =
  let e = Ast_helper.Exp.ident (mknoloc lid) in
  check_wildcard id e

let match_equal equal a b =
  if not (equal a b) then
    raise DontMatch

let try_match f x =
  let w = !wildcards in
  try f x; true
  with DontMatch -> wildcards := w; false

let one_of f l =
  if not (List.exists (fun x -> try_match f x) l) then raise DontMatch

let match_set f ts ps =
  let ok = Hashtbl.create 8 in
  let f t p = f t p; Hashtbl.add ok p () in
  List.iter (fun t -> one_of (f t) ps) ts;
  List.iter (fun p -> if not (Hashtbl.mem ok p) then raise DontMatch) ps

let rec path_matches_lident p l =
  match p, l with
  | _, Lident "__" ->
      true
  | Path.Pdot (p0, s1), Ldot (l0, {txt=s2; _}) when s1 = s2 || s2 = "__" ->
      path_matches_lident p0 l0.txt
  | Path.Pdot (_, s1), Lident s2 when s1 = s2 ->
      true  (* the longident can be a suffix of the path *)
  | Path.Pident id, Lident s ->
      Ident.name id = s
  | _ ->
      false

let rec constructor_match t p =
  match t, p with
  | _, Lident "__" -> ()
  | _, Lident s when is_wildcard s -> check_wildcard_lid s t
  | Lident s1, Lident s2 when s1 = s2 -> ()
  | Ldot (_, {txt=s1; _}), Lident s2 when s1 = s2 -> () (* the ident can be a suffix *)
  | Lident s1, Ldot (_, {txt=s2; _}) when s1 = s2 -> ()
  | Ldot (t, s1), Ldot (p, s2) when s1.txt = s2.txt ->
      constructor_match t.txt p.txt
  | _ -> raise DontMatch

let remove_loc =
  let super = Ast_mapper.default_mapper in
  {super with location = (fun _ _ -> Location.none);
              attributes = (fun _ _ -> [])}

let match_opt f t p =
  match t, p with
  | None, None -> ()
  | Some _, None | None, Some _ -> raise DontMatch
  | Some t, Some p -> f t p

let match_list f t p =
  if List.compare_lengths t p = 0 then List.iter2 f t p
  else raise DontMatch

let match_string : string -> string -> unit = match_equal String.equal

let match_label : string option -> string option -> unit =
  match_opt match_string

(* As of ocaml 5.4, labeled tuple expressions may not be reordered, so we
   match each labeled pair in order and require the labels to agree. *)
let match_labeled f (t_lbl, t) (p_lbl, p) =
  match_label t_lbl p_lbl;
  f t p

let tconstant_equal_pconst tconst pconst =
  match Typecore.constant pconst with
  | Error _ -> false
  | Ok pconst -> Parmatch.const_compare tconst pconst = 0

let rec match_expr texpr (pexpr : Parsetree.expression) =
  if texpr.exp_loc.loc_ghost && not pexpr.pexp_loc.loc_ghost
  then raise DontMatch;

  match texpr.exp_desc, pexpr.pexp_desc with
  (* __ matches any expression *)
  | _, Pexp_ident {txt=Lident "__"; _} ->
      ()

  (* __1234 matches any expression, and checks equality *)
  | _, Pexp_ident {txt=Lident id; _} when is_wildcard id ->
      let e = remove_loc.expr remove_loc (Untypeast.(default_mapper.expr default_mapper texpr)) in
      check_wildcard id e

  | Texp_ident (path, _, _), Pexp_ident {txt=lid; _}
    when path_matches_lident path lid ->
      ()

  | Texp_tuple texprs, Pexp_tuple pexprs ->
      (* as of ocaml 5.4, labeled tuple expressions may not be reordered
         so they must match as-is without sorting *)
      match_list
        (match_labeled match_expr)
        texprs pexprs

  | Texp_array (_, texprs), Pexp_array pexprs ->
      match_exprs texprs pexprs

  | Texp_constant tconst, Pexp_constant pconst when tconstant_equal_pconst tconst pconst ->
      ()

  | Texp_apply (tapply_expr, targs), Pexp_apply (pexpr, pargs) ->
      match_expr tapply_expr pexpr;
      let rec check_all targs = function
        | [] -> () (* ok if more arguments in the typed expression *)
        | (Asttypes.Optional _ as lab, {pexp_desc=Pexp_construct({txt=Lident ("MISSING"|"PRESENT" as cstr); _}, None); _}) :: pargs ->
            let pr = cstr = "PRESENT" in
            let rec loop = function
              | [] -> raise DontMatch
              | (l, Arg targ) :: targs when l = lab ->
                  if pr = targ.exp_loc.loc_ghost then raise DontMatch;
                  targs
              | x :: targs -> x :: loop targs
            in
            check_all (loop targs) pargs

        | (lab, parg) :: pargs ->
            let rec loop = function
              | [] -> raise DontMatch
              | (l, Arg targ) :: targs when l = lab ->
                  match_expr targ parg; targs
              | (Asttypes.Optional _ as l, Arg {exp_desc=Texp_construct({txt=Lident "Some"; _}, _, [targ]); _}) :: targs when l = lab ->
                  match_expr targ parg; targs
              | x :: targs -> x :: loop targs
            in
            check_all (loop targs) pargs
      in
      check_all targs pargs
  | Texp_function ([{fp_arg_label = Nolabel; _}], Tfunction_cases {cases = tcases; _}), Pexp_function ([{pparam_desc = Pparam_val (Nolabel, None, _); _}], _, Pfunction_cases (pcases, _, _)) ->
      match_cases tcases pcases

  | Texp_construct (tcstr, _tconstr_desc, texprs), Pexp_construct (pcstr, pexpr_opt) ->
      constructor_match tcstr.txt pcstr.txt;
      begin match pexpr_opt, texprs with
      | Some {pexp_desc = Pexp_ident {txt = Lident "__"; _}; _}, _ -> ()
      | None, [] -> ()
      | Some {pexp_desc = Pexp_tuple pexprs; _}, _ :: _ :: _ ->
          (* The typed args of a constructor are an unlabeled list, so we
             drop the labels of the parsetree tuple. *)
          let pexprs = List.map snd pexprs in
          match_exprs texprs pexprs
      | Some pexpr, [ texpr ] -> match_expr texpr pexpr
      | _ -> raise DontMatch
      end

  | Texp_variant (tl, te), Pexp_variant (pl, pe) when tl = pl ->
      match_opt match_expr te pe

  | Texp_match (te, tcases, _teffects, _), Pexp_match (pe, pcases) ->
      (* TODO: split effects from the other cases in the parsetree *)
      match_expr te pe;
      match_cases tcases pcases

  | Texp_try (te, tcases, _teffects), Pexp_try (pe, pcases) ->
      (* TODO: split effects from the other cases in the parsetree *)
      match_expr te pe;
      match_cases tcases pcases

  | Texp_let (trf, tvb, te), Pexp_let (prf, pvb, pe) when trf = prf ->
      match_expr te pe;
      match_value_bindings tvb pvb

  | Texp_ifthenelse (te1, te2, te3), Pexp_ifthenelse (pe1, pe2, pe3) ->
      match_expr te1 pe1;
      match_expr te2 pe2;
      match_opt match_expr te3 pe3

  | Texp_sequence (te1, te2), Pexp_sequence (pe1, pe2)
  | Texp_while (te1, te2), Pexp_while (pe1, pe2) ->
      match_expr te1 pe1;
      match_expr te2 pe2

  | Texp_assert (te, _), Pexp_assert pe
  | Texp_lazy te, Pexp_lazy pe ->
      match_expr te pe

  | Texp_field (texpr, tid, _), Pexp_field (pexpr, pid)  ->
      constructor_match tid.txt pid.txt;
      match_expr texpr pexpr

  | Texp_setfield (te1, tid, _, te2), Pexp_setfield (pe1, pid, pe2) ->
      constructor_match tid.txt pid.txt;
      match_expr te1 pe1;
      match_expr te2 pe2

  | Texp_setfield (te1, tid, _, _), Pexp_field (pexpr, pid) ->
      constructor_match tid.txt pid.txt;
      match_expr te1 pexpr

  | _, Pexp_constraint (pe, pt) ->
      match_expr texpr pe;
      if not (match_typ texpr.exp_type pt) then raise DontMatch

  | Texp_record {fields = tfields; extended_expression = tdef; _}, Pexp_record (pfields, pdef) ->
      match_opt match_expr tdef pdef;
      let f (tid, _, te) (pid, pe) =
        constructor_match tid.txt pid.txt;
        match_expr te pe
      in
      let tfields = List.filter_map (function
          | (_, Kept _) -> None
          | (lbl, Overridden (id, e)) -> Some (id, lbl, e)
        ) (Array.to_list tfields)
      in
      match_set f tfields pfields

  | Texp_send (te, Tmeth_name ts), Pexp_send (pe, {txt = ps; _}) when ts = ps ->
      match_expr te pe
  | Texp_send (te, Tmeth_val id), Pexp_send (pe, {txt = ps; _}) when Ident.name id = ps ->
      match_expr te pe

  | Texp_new (path, _, _), Pexp_new lid when path_matches_lident path lid.txt ->
      ()

  | Texp_for (tident, patident, texpr1, texpr2, tdir_flag, texpr), Pexp_for (pident, pexpr1, pexpr2, pdir_flag, pexpr)  when tdir_flag = pdir_flag ->
      begin match patident.ppat_desc, pident.ppat_desc with
      | Ppat_any, Ppat_any -> ()
      | Ppat_any, Ppat_var {txt = "__"; loc = _} -> ()
      | Ppat_var {txt; loc = _}, Ppat_any when String.starts_with ~prefix:"_" txt -> ()
      | Ppat_var _, Ppat_var {txt; loc = _} when path_matches_lident (Path.Pident tident) (Longident.Lident txt) -> ()
      | _ -> raise DontMatch
      end;
      match_expr texpr1 pexpr1;
      match_expr texpr2 pexpr2;
      match_expr texpr pexpr;

  | _ ->
      raise DontMatch

and match_typ texpr ptyp =
  match parse_type ptyp with
  | typ ->
      let env = Lazy.force initial_env in
      begin try Ctype.is_moregeneral env false typ texpr
      with Assert_failure _ ->
        (* When dealing with inline records [moregeneral] above calls
           [Env.find_type_full] in a context that should never occur in the
           typechecker which causes assert false to be thrown. *)
        false
      end
  | exception _ ->
      begin match Types.get_desc texpr, ptyp.Parsetree.ptyp_desc with
      | Tconstr (path, ty_args, _), Ptyp_constr ({Location.txt; loc = _}, pty_args) ->
          if path_matches_lident path txt then begin
            match pty_args with
            | [{ptyp_desc = Ptyp_constr({Location.txt = Lident "__"; loc = _}, []); _}] -> true
            | _ ->
                if List.length ty_args = List.length pty_args then
                  List.for_all2 match_typ ty_args pty_args
                else false
          end else false
      | _ -> false
      end

and match_pat : type k. k general_pattern -> _ -> _ = fun tpat ppat ->
  match tpat.pat_desc, ppat.ppat_desc with
  | Tpat_any, Ppat_any -> ()
  | _, Ppat_var {txt = "__"; _} -> ()
  | Tpat_var (_, {txt = s1; _}, _), Ppat_var {txt = s2; _} when is_wildcard s2 ->
      check_wildcard_lid s2 (Lident s1)
  | Tpat_var (_, {txt = s1; _}, _), Ppat_var {txt = s2; _} when s1 = s2 -> ()
  | Tpat_tuple tl, Ppat_tuple (pl, _closed_flag) ->
      (* As of ocaml 5.4, both pattern tuples carry optional labels.
         Match them in order, requiring labels to agree. We ignore the
         closed_flag for now: a query of `(a, b)` will still match a typed
         tuple of length 2. *)
      match_list (match_labeled match_pat) tl pl
  | Tpat_constant tc, Ppat_constant pc when tconstant_equal_pconst tc pc -> ()
  | Tpat_construct (tcstr, _tconstr_desc, tpats, _), Ppat_construct (pcstr, ppat_opt) ->
      constructor_match tcstr.txt pcstr.txt;
      begin match ppat_opt, tpats with
      | None, [] -> ()
      | Some (_, {ppat_desc = Ppat_tuple (ppats, _closed_flag); _}), _ :: _ :: _ ->
          (* Typed constructor args are an unlabeled list, so drop the
             labels from the parsetree tuple. *)
          let ppats = List.map snd ppats in
          match_list match_pat tpats ppats
      | Some (_, ppat), [ tpat ] -> match_pat tpat ppat
      | _ -> raise DontMatch
      end
  | _, Ppat_constraint (ppat, pt) ->
      match_pat tpat ppat;
      let pt = parse_type pt in
      let env = Lazy.force initial_env in
      let eq = Ctype.is_moregeneral env false pt tpat.pat_type in
      if not eq then raise DontMatch
  | Tpat_or (t1, t2, _), Ppat_or (p1, p2) ->
      match_pat t1 p1;
      match_pat t2 p2
  | Tpat_value t, _ ->
      match_pat (t :> value general_pattern) ppat
  | _ -> raise DontMatch

and match_pat_expr : type k. k general_pattern -> _ -> _ = fun tpat pexpr ->
  match tpat.pat_desc, pexpr.pexp_desc with
  | Tpat_record (fields, _), Pexp_field ({pexp_desc = Pexp_ident {txt=Lident "__"; _}; _}, {txt = Lident s; _}) ->
      if not (List.exists (fun (_, {Data_types.lbl_name; _}, _) -> lbl_name = s) fields) then
        raise DontMatch
  | _ ->
      raise DontMatch

and match_exprs texprs pexprs =
  match_list match_expr texprs pexprs

and match_cases : type k. k case list -> _ -> _ = fun tcases pcases ->
  match_set match_case tcases pcases

and match_value_bindings t p =
  match_set match_value_binding t p

and match_value_binding
    {vb_pat; vb_expr; vb_attributes = _; vb_loc = _; vb_rec_kind = _}
    {pvb_pat; pvb_expr; pvb_attributes = _; pvb_loc = _; pvb_constraint = _} =
  match_expr vb_expr pvb_expr;
  match_pat vb_pat pvb_pat

and match_case : type k. k case -> _ -> _ = fun {c_lhs; c_guard; c_rhs; _} {pc_lhs; pc_guard; pc_rhs} ->
  match_pat c_lhs pc_lhs;
  match_opt match_expr c_guard pc_guard;
  match_expr c_rhs pc_rhs

let search_cmt cmt query_expr =
  let open Cmt_format in
  let res = ref [] in
  let cmt_search =
    let open Tast_iterator in
    let super = default_iterator in
    let pat : type k. _ -> k general_pattern -> _ = fun self p ->
      try
        match_pat_expr p query_expr;
        res := p.Typedtree.pat_loc :: !res
      with DontMatch ->
        super.pat self p
    in
    let expr self e =
      wildcards := [];
      try
        match_expr e query_expr;
        res := e.Typedtree.exp_loc :: !res
      with DontMatch ->
        super.expr self e
    in
    {super with expr; pat}
  in
  begin match cmt.cmt_annots with
  | Implementation str -> cmt_search.Tast_iterator.structure cmt_search str
  | Interface sg -> cmt_search.Tast_iterator.signature cmt_search sg
  | _ -> ()
  end;
  List.sort Stdlib.compare !res
