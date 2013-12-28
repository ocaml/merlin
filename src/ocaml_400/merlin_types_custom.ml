open Std
open Misc

let include_idents l = l

let lookup_constructor id env = snd (Env.lookup_constructor id env)
let lookup_label id env = snd (Env.lookup_label id env)
let fold_types = Env.fold_types

let fold_constructors f =
  Env.fold_constructors (fun name _ descr -> f name descr)
let fold_labels f = Env.fold_labels (fun _ _ -> f)

let extract_subpatterns =
  let open Typedtree in function
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> []
  | Tpat_alias (p,_,_) | Tpat_lazy p | Tpat_variant (_,Some p,_) -> [p]
  | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,_,ps,_) -> ps
  | Tpat_or (p1,p2,_) -> [p1;p2]
  | Tpat_record (r,_) -> List.map ~f:(fun (_,_,_,p) -> p) r

let extract_specific_subexpressions =
  let open Typedtree in function
  | Texp_construct (_,_,_,es,_)  -> es
  | Texp_record (pldes,Some e) -> e :: List.map ~f:fth4 pldes
  | Texp_record (pldes,None)   -> List.map ~f:fth4 pldes
  | Texp_field (ea,_,_,_)        -> [ea]
  | Texp_setfield (ea,_,_,_,eb)  -> [ea;eb]
  | _ -> assert false

let exp_open_env = function
  | Typedtree.Texp_open (_,_,env) -> env
  | _ -> assert false
