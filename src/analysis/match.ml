(* This file is part of the ocamlgrep package
   See the attached LICENSE file.
   Copyright (C) 2026 LexiFi

   Originally written by Nicolás Ojeda Bär (LexiFi);
   maintained by Martin Jambon (LexiFi). *)
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

let match_set f ps ts =
  let ok = Hashtbl.create 8 in
  let f t p = f p t; Hashtbl.add ok p () in
  List.iter (fun t -> one_of (f t) ps) ts;
  List.iter (fun p -> if not (Hashtbl.mem ok p) then raise DontMatch) ps

let rec path_matches_lident l p =
  match l, p with
  | Lident "__", _ ->
      true
  | Ldot (l0, {txt=s2; _}), Path.Pdot (p0, s1) when s1 = s2 || s2 = "__" ->
      path_matches_lident l0.txt p0
  | Lident s2, Path.Pdot (_, s1) when s1 = s2 ->
      true  (* the longident can be a suffix of the path *)
  | Lident s, Path.Pident id ->
      Ident.name id = s
  | _ ->
      false

let rec constructor_match p t =
  match p, t with
  | Lident "__", _ -> ()
  | Lident s, _ when is_wildcard s -> check_wildcard_lid s t
  | Lident s2, Lident s1 when s1 = s2 -> ()
  | Lident s2, Ldot (_, {txt=s1; _}) when s1 = s2 -> () (* the ident can be a suffix *)
  | Ldot (_, {txt=s2; _}), Lident s1 when s1 = s2 -> ()
  | Ldot (p, s2), Ldot (t, s1) when s1.txt = s2.txt ->
      constructor_match p.txt t.txt
  | _ -> raise DontMatch

let remove_loc =
  let super = Ast_mapper.default_mapper in
  {super with location = (fun _ _ -> Location.none);
              attributes = (fun _ _ -> [])}

let match_opt f p t =
  match p, t with
  | None, None -> ()
  | None, Some _ | Some _, None -> raise DontMatch
  | Some p, Some t -> f p t

let match_list f p t =
  if List.compare_lengths p t = 0 then List.iter2 f p t
  else raise DontMatch

let match_string : string -> string -> unit = match_equal String.equal

let match_label : string option -> string option -> unit =
  match_opt match_string

(* As of ocaml 5.4, labeled tuple expressions may not be reordered, so we
   match each labeled pair in order and require the labels to agree. *)
let match_labeled f (p_lbl, p) (t_lbl, t) =
  match_label p_lbl t_lbl;
  f p t

let tconstant_equal_pconst tconst pconst =
  match Typecore.constant pconst with
  | Error _ -> false
  | Ok pconst -> Parmatch.const_compare tconst pconst = 0

let rec match_expr (pexpr : Parsetree.expression) texpr =
  if texpr.exp_loc.loc_ghost && not pexpr.pexp_loc.loc_ghost
  then raise DontMatch;

  match pexpr.pexp_desc, texpr.exp_desc with
  (* __ matches any expression *)
  | Pexp_ident {txt=Lident "__"; _}, _ ->
      ()

  (* __1234 matches any expression, and checks equality *)
  | Pexp_ident {txt=Lident id; _}, _ when is_wildcard id ->
      let e = remove_loc.expr remove_loc (Untypeast.(default_mapper.expr default_mapper texpr)) in
      check_wildcard id e

  | Pexp_ident {txt=lid; _}, Texp_ident (path, _, _)
    when path_matches_lident lid path ->
      ()

  | Pexp_tuple pexprs, Texp_tuple texprs ->
      (* as of ocaml 5.4, labeled tuple expressions may not be reordered
         so they must match as-is without sorting *)
      match_list
        (match_labeled match_expr)
        pexprs texprs

  | Pexp_array pexprs, Texp_array (_, texprs) ->
      match_exprs pexprs texprs

  | Pexp_constant pconst, Texp_constant tconst when tconstant_equal_pconst tconst pconst ->
      ()

  | Pexp_apply (pexpr, pargs), Texp_apply (tapply_expr, targs) ->
      match_expr pexpr tapply_expr;
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
                  match_expr parg targ; targs
              | (Asttypes.Optional _ as l, Arg {exp_desc=Texp_construct({txt=Lident "Some"; _}, _, [targ]); _}) :: targs when l = lab ->
                  match_expr parg targ; targs
              | x :: targs -> x :: loop targs
            in
            check_all (loop targs) pargs
      in
      check_all targs pargs
  | Pexp_function ([{pparam_desc = Pparam_val (Nolabel, None, _); _}], _, Pfunction_cases (pcases, _, _)), Texp_function ([{fp_arg_label = Nolabel; _}], Tfunction_cases {cases = tcases; _}) ->
      match_cases pcases tcases

  | Pexp_construct (pcstr, pexpr_opt), Texp_construct (tcstr, _tconstr_desc, texprs) ->
      constructor_match pcstr.txt tcstr.txt;
      begin match pexpr_opt, texprs with
      | Some {pexp_desc = Pexp_ident {txt = Lident "__"; _}; _}, _ -> ()
      | None, [] -> ()
      | Some {pexp_desc = Pexp_tuple pexprs; _}, _ :: _ :: _ ->
          (* The typed args of a constructor are an unlabeled list, so we
             drop the labels of the parsetree tuple. *)
          let pexprs = List.map snd pexprs in
          match_exprs pexprs texprs
      | Some pexpr, [ texpr ] -> match_expr pexpr texpr
      | _ -> raise DontMatch
      end

  | Pexp_variant (pl, pe), Texp_variant (tl, te) when tl = pl ->
      match_opt match_expr pe te

  | Pexp_match (pe, pcases), Texp_match (te, tcases, _teffects, _) ->
      (* TODO: split effects from the other cases in the parsetree *)
      match_expr pe te;
      match_cases pcases tcases

  | Pexp_try (pe, pcases), Texp_try (te, tcases, _teffects) ->
      (* TODO: split effects from the other cases in the parsetree *)
      match_expr pe te;
      match_cases pcases tcases

  | Pexp_let (prf, pvb, pe), Texp_let (trf, tvb, te) when trf = prf ->
      match_expr pe te;
      match_value_bindings pvb tvb

  | Pexp_ifthenelse (pe1, pe2, pe3), Texp_ifthenelse (te1, te2, te3) ->
      match_expr pe1 te1;
      match_expr pe2 te2;
      match_opt match_expr pe3 te3

  | Pexp_sequence (pe1, pe2), Texp_sequence (te1, te2)
  | Pexp_while (pe1, pe2), Texp_while (te1, te2) ->
      match_expr pe1 te1;
      match_expr pe2 te2

  | Pexp_assert pe, Texp_assert (te, _)
  | Pexp_lazy pe, Texp_lazy te ->
      match_expr pe te

  | Pexp_field (pexpr, pid), Texp_field (texpr, tid, _) ->
      constructor_match pid.txt tid.txt;
      match_expr pexpr texpr

  | Pexp_setfield (pe1, pid, pe2), Texp_setfield (te1, tid, _, te2) ->
      constructor_match pid.txt tid.txt;
      match_expr pe1 te1;
      match_expr pe2 te2

  | Pexp_field (pexpr, pid), Texp_setfield (te1, tid, _, _) ->
      constructor_match pid.txt tid.txt;
      match_expr pexpr te1

  | Pexp_constraint (pe, pt), _ ->
      match_expr pe texpr;
      if not (match_typ pt texpr.exp_type) then raise DontMatch

  | Pexp_record (pfields, pdef), Texp_record {fields = tfields; extended_expression = tdef; _} ->
      match_opt match_expr pdef tdef;
      let f (pid, pe) (tid, _, te) =
        constructor_match pid.txt tid.txt;
        match_expr pe te
      in
      let tfields = List.filter_map (function
          | (_, Kept _) -> None
          | (lbl, Overridden (id, e)) -> Some (id, lbl, e)
        ) (Array.to_list tfields)
      in
      match_set f pfields tfields

  | Pexp_send (pe, {txt = ps; _}), Texp_send (te, Tmeth_name ts) when ts = ps ->
      match_expr pe te
  | Pexp_send (pe, {txt = ps; _}), Texp_send (te, Tmeth_val id) when Ident.name id = ps ->
      match_expr pe te

  | Pexp_new lid, Texp_new (path, _, _) when path_matches_lident lid.txt path ->
      ()

  | Pexp_for (pident, pexpr1, pexpr2, pdir_flag, pexpr), Texp_for (tident, patident, texpr1, texpr2, tdir_flag, texpr) when tdir_flag = pdir_flag ->
      begin match pident.ppat_desc, patident.ppat_desc with
      | Ppat_any, Ppat_any -> ()
      | Ppat_var {txt = "__"; loc = _}, Ppat_any -> ()
      | Ppat_any, Ppat_var {txt; loc = _} when String.starts_with ~prefix:"_" txt -> ()
      | Ppat_var {txt; loc = _}, Ppat_var _ when path_matches_lident (Longident.Lident txt) (Path.Pident tident) -> ()
      | _ -> raise DontMatch
      end;
      match_expr pexpr1 texpr1;
      match_expr pexpr2 texpr2;
      match_expr pexpr texpr;

  | _ ->
      raise DontMatch

and match_typ ptyp texpr =
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
      begin match ptyp.Parsetree.ptyp_desc, Types.get_desc texpr with
      | Ptyp_constr ({Location.txt; loc = _}, pty_args), Tconstr (path, ty_args, _) ->
          if path_matches_lident txt path then begin
            match pty_args with
            | [{ptyp_desc = Ptyp_constr({Location.txt = Lident "__"; loc = _}, []); _}] -> true
            | _ ->
                if List.length ty_args = List.length pty_args then
                  List.for_all2 match_typ pty_args ty_args
                else false
          end else false
      | _ -> false
      end

and match_pat : type k. _ -> k general_pattern -> _ = fun ppat tpat ->
  match ppat.ppat_desc, tpat.pat_desc with
  | Ppat_any, Tpat_any -> ()
  | Ppat_var {txt = "__"; _}, _ -> ()
  | Ppat_var {txt = s2; _}, Tpat_var (_, {txt = s1; _}, _) when is_wildcard s2 ->
      check_wildcard_lid s2 (Lident s1)
  | Ppat_var {txt = s2; _}, Tpat_var (_, {txt = s1; _}, _) when s1 = s2 -> ()
  | Ppat_tuple (pl, _closed_flag), Tpat_tuple tl ->
      (* As of ocaml 5.4, both pattern tuples carry optional labels.
         Match them in order, requiring labels to agree. We ignore the
         closed_flag for now: a query of `(a, b)` will still match a typed
         tuple of length 2. *)
      match_list (match_labeled match_pat) pl tl
  | Ppat_constant pc, Tpat_constant tc when tconstant_equal_pconst tc pc -> ()
  | Ppat_construct (pcstr, ppat_opt), Tpat_construct (tcstr, _tconstr_desc, tpats, _) ->
      constructor_match pcstr.txt tcstr.txt;
      begin match ppat_opt, tpats with
      | None, [] -> ()
      | Some (_, {ppat_desc = Ppat_tuple (ppats, _closed_flag); _}), _ :: _ :: _ ->
          (* Typed constructor args are an unlabeled list, so drop the
             labels from the parsetree tuple. *)
          let ppats = List.map snd ppats in
          match_list match_pat ppats tpats
      | Some (_, ppat), [ tpat ] -> match_pat ppat tpat
      | _ -> raise DontMatch
      end
  | Ppat_constraint (ppat, pt), _ ->
      match_pat ppat tpat;
      let pt = parse_type pt in
      let env = Lazy.force initial_env in
      let eq = Ctype.is_moregeneral env false pt tpat.pat_type in
      if not eq then raise DontMatch
  | Ppat_or (p1, p2), Tpat_or (t1, t2, _) ->
      match_pat p1 t1;
      match_pat p2 t2
  | _, Tpat_value t ->
      match_pat ppat (t :> value general_pattern)
  | _ -> raise DontMatch

and match_pat_expr : type k. _ -> k general_pattern -> _ = fun pexpr tpat ->
  match pexpr.pexp_desc, tpat.pat_desc with
  | Pexp_field ({pexp_desc = Pexp_ident {txt=Lident "__"; _}; _}, {txt = Lident s; _}), Tpat_record (fields, _) ->
      if not (List.exists (fun (_, {Data_types.lbl_name; _}, _) -> lbl_name = s) fields) then
        raise DontMatch
  | _ ->
      raise DontMatch

and match_exprs pexprs texprs =
  match_list match_expr pexprs texprs

and match_cases : type k. _ -> k case list -> _ = fun pcases tcases ->
  match_set match_case pcases tcases

and match_value_bindings p t =
  match_set match_value_binding p t

and match_value_binding
    {pvb_pat; pvb_expr; pvb_attributes = _; pvb_loc = _; pvb_constraint = _}
    {vb_pat; vb_expr; vb_attributes = _; vb_loc = _; vb_rec_kind = _} =
  match_expr pvb_expr vb_expr;
  match_pat pvb_pat vb_pat

and match_case : type k. _ -> k case -> _ = fun {pc_lhs; pc_guard; pc_rhs} {c_lhs; c_guard; c_rhs; _} ->
  match_pat pc_lhs c_lhs;
  match_opt match_expr pc_guard c_guard;
  match_expr pc_rhs c_rhs

type finding = {
  loc : Location.t;
  lines : string list;
}

let parse_query query =
  (* Use Merlin's vendored parser ([Parser_raw.parse_expression]) rather than
     upstream [Parse.implementation], following the pattern in
     [src/analysis/type_utils.ml]. *)
  let lexbuf = Lexing.from_string query in
  let state = Lexer_raw.make (Lexer_raw.keywords []) in
  let rec lexer = function
    | Lexer_raw.Fail (e, l) -> raise (Lexer_raw.Error (e, l))
    | Lexer_raw.Return token -> token
    | Lexer_raw.Refill k -> lexer (k ())
  in
  let lexer lexbuf = lexer (Lexer_raw.token_without_comments state lexbuf) in
  try Parser_raw.parse_expression lexer lexbuf
  with _ -> failwith "Could not parse search expression."

let search_cmt query_expr cmt =
  let open Cmt_format in
  let res = ref [] in
  let cmt_search =
    let open Tast_iterator in
    let super = default_iterator in
    let pat : type k. _ -> k general_pattern -> _ = fun self p ->
      try
        match_pat_expr query_expr p;
        res := p.Typedtree.pat_loc :: !res
      with DontMatch ->
        super.pat self p
    in
    let expr self e =
      wildcards := [];
      try
        match_expr query_expr e;
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

let search query_expr cmt ~source ~src_lines =
  let nb_lines = Array.length src_lines in
  let with_fname (pos : Lexing.position) = { pos with pos_fname = source } in
  List.filter_map
    (fun ({ Location.loc_start; loc_end; loc_ghost } : Location.t) ->
      let s = max 1 (min nb_lines loc_start.pos_lnum) in
      let e = max s (min nb_lines loc_end.pos_lnum) in
      let lines = List.init (e - s + 1) (fun k -> src_lines.(s - 1 + k)) in
      let loc =
        { Location.loc_start = with_fname loc_start;
          loc_end = with_fname loc_end;
          loc_ghost }
      in
      Some { loc; lines })
    (search_cmt query_expr cmt)
