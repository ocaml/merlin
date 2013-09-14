open Std
open Misc

let signature_item_ident = 
  let open Types in function
  | Sig_value (id, _)
  | Sig_type (id, _, _)
  | Sig_exception (id, _)
  | Sig_module (id, _, _)
  | Sig_modtype (id, _)
  | Sig_class (id, _, _)
  | Sig_class_type (id, _, _) -> id

let include_idents l = List.map signature_item_ident l

let lookup_constructor = Env.lookup_constructor
let lookup_label       = Env.lookup_label

let fold_types f id env acc =
  Env.fold_types (fun s p (decl,descr) acc -> f s p decl acc) id env acc

let fold_constructors f id env acc =
  Env.fold_constructors 
    (fun constr acc -> f constr.Types.cstr_name constr acc)
    id env acc
let fold_labels = Env.fold_labels

let extract_subpatterns =
  let open Typedtree in function
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> []
  | Tpat_alias (p,_,_) | Tpat_lazy p | Tpat_variant (_,Some p,_) -> [p]
  | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,ps,_) -> ps
  | Tpat_or (p1,p2,_) -> [p1;p2]
  | Tpat_record (r,_) -> List.map ~f:thd3 r

let extract_specific_subexpressions =
  let open Typedtree in function
  | Texp_construct (_,_,es,_)  -> es
  | Texp_record (pldes,Some e) -> e :: List.map ~f:thd3 pldes
  | Texp_record (pldes,None)   -> List.map ~f:thd3 pldes
  | Texp_field (ea,_,_)        -> [ea]
  | Texp_setfield (ea,_,_,eb)  -> [ea;eb]
  | _ -> assert false

let exp_open_env = function
  | Typedtree.Texp_open (_,_,_,env) -> env
  | _ -> assert false
