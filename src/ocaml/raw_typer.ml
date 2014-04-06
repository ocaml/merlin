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

let step v (is_rec,_) = match v with
  | Terminal _ | Bottom -> empty
  | Nonterminal nt ->
    match nt with
    | NT'rec_flag r -> r, []
    | NT'implementation str | NT'structure str
    | NT'structure_tail str | NT'structure_item str ->
      default, [Structure str]
    | NT'strict_binding e | NT'simple_expr e | NT'seq_expr e
    | NT'opt_default (Some e) | NT'fun_def e | NT'fun_binding e | NT'expr e
    | NT'labeled_simple_expr (_,e) | NT'label_ident (_,e)
    | NT'label_expr (_,e) ->
      default, [Eval e]
    | NT'let_bindings e ->
      default, [Bindings (is_rec,e)]
    (*| NT'let_rec_bindings e -> `binds e*)
    | NT'expr_semi_list el | NT'expr_comma_list el  ->
      default, List.map (fun e -> Eval e) el
    | NT'interface sg | NT'signature_item sg ->
      default, [Signature sg]
    | NT'signature sg ->
      default, [Signature (List.rev sg)]
    (*| NT'module_functor_arg (id,mty) ->
      `fmd (id,mty)*)
    | NT'labeled_simple_pattern pat ->
      default, [Pattern pat]
    | NT'pattern pat ->
      default, [Pattern ("",None,pat)]
    | _ -> empty

