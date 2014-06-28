open Location
open Parsetree
open Raw_parser

type item =
  | Structure of structure
  | Signature of signature
  | Pattern of (Asttypes.label * expression option * pattern)
  | Eval of expression
  | Bindings of Asttypes.rec_flag * (pattern * expression) list
  | Newtype of string
  (* | Functor_argument of string loc * module_type option *)
  | Open of Asttypes.override_flag * Longident.t loc

type t = Asttypes.rec_flag * item list
let default = Asttypes.Nonrecursive
let empty = default, []
let observe = snd

let step_nt (type a) is_rec (nt : a nonterminal_class) (v : a) =
  let mk_fun cases =
    { pexp_desc = (Pexp_function ("", None, cases)) ; pexp_loc = Location.none }
  in
  match nt, v with
  | N_rec_flag, r                 -> (r : Asttypes.rec_flag), []
  | N_implementation, str         -> default, [Structure str]
  | N_structure, str              -> default, [Structure str]
  | N_structure_tail, str         -> default, [Structure str]
  | N_structure_item, str         -> default, [Structure str]
  | N_strict_binding, e           -> default, [Eval e]
  | N_simple_expr, e              -> default, [Eval e]
  | N_seq_expr, e                 -> default, [Eval e]
  | N_opt_default, (Some e)       -> default, [Eval e]
  | N_fun_def, e                  -> default, [Eval e]
  | N_expr, e                     -> default, [Eval e]
  | N_labeled_simple_expr, (_,e)  -> default, [Eval e]
  | N_label_ident, (_,e)          -> default, [Eval e]
  | N_label_expr, (_,e)           -> default, [Eval e]
  | N_let_bindings, e             -> default, [Bindings (is_rec,e)]
  (*| N_let_rec_bindings, e -> `binds e*)
  | N_expr_semi_list, el          -> default, List.map (fun e -> Eval e) el
  | N_expr_comma_list, el         -> default, List.map (fun e -> Eval e) el
  | N_interface, sg               -> default, [Signature sg]
  | N_signature_item, sg          -> default, [Signature sg]
  | N_signature, sg               -> default, [Signature (List.rev sg)]
  (*| N_module_functor_arg, (id,mty) -> `fmd (id,mty)*)
  | N_labeled_simple_pattern, pat -> default, [Pattern pat]
  | N_pattern, pat                -> default, [Pattern ("",None,pat)]
  | N_match_cases, cases          -> default, [Eval (mk_fun cases)]
  | _                             -> empty

let step v (is_rec,_) = match v with
  | T_ _ | Bottom -> empty
  | N_ (nt,v) -> step_nt is_rec nt v

let dump_item ppf = function
  | Structure str -> Printast.implementation ppf str
  | Signature sg -> Printast.interface ppf sg
  | Pattern _ -> ()
  | Eval _ -> ()
  | Bindings _ -> ()
  | Newtype _ -> ()
  (* | Functor_argument _ -> () *)
  | Open _ -> ()

let dump ppf t =
  List.iter (dump_item ppf) (observe t)

let fresh_env () = Env.initial

let rewrite loc = function
  (*
  | Functor_argument (id,mty) ->
    let mexpr = Pmod_structure [] in
    let mexpr = {pmod_desc = mexpr; pmod_loc = loc} in
    let mexpr = Pmod_functor (id, mty, mexpr) in
    let mexpr = {pmod_desc = mexpr; pmod_loc = loc} in
    let item = Pstr_module (Ast_helper.Mb.mk (Location.mknoloc "") mexpr) in
    `fake { pstr_desc = item; pstr_loc = loc }
  *)
  | Pattern (l,o,p) ->
    let expr = Pexp_constant (Asttypes.Const_int 0) in
    let expr = { pexp_desc = expr; pexp_loc = loc } in
    let expr = Pexp_function (l, o, [(p, expr)]) in
    let expr = { pexp_desc = expr; pexp_loc = loc } in
    let item = Pstr_eval expr in
    `fake { pstr_desc = item; pstr_loc = loc }
  | Newtype s ->
    let expr = Pexp_constant (Asttypes.Const_int 0) in
    let expr = { pexp_desc = expr; pexp_loc = Location.none } in
    let pat = { ppat_desc = Ppat_any; ppat_loc = Location.none } in
    let expr = Pexp_function ("", None, [(pat, expr)]) in
    let expr = { pexp_desc = expr; pexp_loc = Location.none } in
    let expr = Parsetree.Pexp_newtype (s,expr) in
    let expr = { pexp_desc = expr; pexp_loc = loc } in
    let item = Pstr_eval expr in
    `fake { pstr_desc = item; pstr_loc = loc }
  | Bindings (rec_,e) ->
    let item = Pstr_value (rec_,e) in
    `str [{ pstr_desc = item; pstr_loc = loc }]
  | Open (override,name) ->
    let item = Pstr_open (override, name) in
    `str [{ pstr_desc = item; pstr_loc = loc }]
  | Eval e ->
    `str [{
      Parsetree. pstr_desc = Parsetree.Pstr_eval e;
      pstr_loc = e.Parsetree.pexp_loc;
    }]
  | Structure str ->
    `str str
  | Signature sg ->
    `sg sg
