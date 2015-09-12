(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open Location
open Parsetree
open Raw_parser

type item =
  | Structure of structure
  | Signature of signature
  | Pattern of (Asttypes.label * expression option * pattern)
  | Eval of expression
  | Bindings of Asttypes.rec_flag * value_binding list
  | Newtype of string
  | Functor_argument of string loc * module_type option
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
  | N_let_bindings, e             ->
    let e = List.map (fun (p,e) -> Ast_helper.Vb.mk p e) e in
    default, [Bindings (is_rec,e)]
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
  | Functor_argument _ -> ()
  | Open _ -> ()

let dump ppf t =
  List.iter (dump_item ppf) (observe t)

let fresh_env () = Env.initial

(* No PPX: trust locations *)
let rewrite_loc t = t
