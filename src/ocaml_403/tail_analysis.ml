open Std
open BrowseT
open Typedtree

let expr_tail_positions = function
  | Texp_instvar _ | Texp_setinstvar _ | Texp_override _ | Texp_assert _
  | Texp_lazy _ | Texp_object _ | Texp_pack _
  | Texp_function _ | Texp_apply _ | Texp_tuple _
  | Texp_ident _ | Texp_constant _
  | Texp_construct _ | Texp_variant _ | Texp_record _
  | Texp_field _ | Texp_setfield _ | Texp_array _
  | Texp_while _ | Texp_for _ | Texp_send _ | Texp_new _
    -> []
  (* Match with no exception handler *)
  | Texp_match (_,cs,[],_)
  | Texp_match (_,_,cs,_) | Texp_try (_,cs)
    -> List.map cs ~f:(fun c -> Case c)
  | Texp_letmodule (_,_,_,e) | Texp_let (_,_,e)
  | Texp_sequence (_,e) | Texp_ifthenelse (_,e,None)
    -> [Expression e]
  | Texp_ifthenelse (_,e1,Some e2)
    -> [Expression e1; Expression e2]

let tail_positions = function
  | Expression expr -> expr_tail_positions expr.exp_desc
  | Case case -> [Expression case.c_rhs]
  | _ -> []

(* If the expression is a function, return all of its entry-points (which are
   in tail-positions). Returns an empty list otherwise *)
let expr_entry_points = function
  | Texp_function (_,cs,_) -> List.map cs ~f:(fun c -> Case c)
  | _ -> []

let entry_points = function
  | Expression expr -> expr_entry_points expr.exp_desc
  | _ -> []

(* FIXME: what about method call? It should be translated to a Texp_apply,
   but I am not sure *)
let is_call = function
  | Expression {exp_desc = Texp_apply _} -> true
  | _ -> false
