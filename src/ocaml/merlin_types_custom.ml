open Std
open Misc

let signature_item_ident =
  let open Types in function
  | Sig_value (id, _)
  | Sig_type (id, _, _)
  | Sig_typext (id, _, _)
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
  | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,ps) -> ps
  | Tpat_or (p1,p2,_) -> [p1;p2]
  | Tpat_record (r,_) -> List.map ~f:thd3 r

let extract_specific_subexpressions =
  let open Typedtree in function
  | Texp_construct (_,_,es)  -> es
  | Texp_record (pldes,Some e) -> e :: List.map ~f:thd3 pldes
  | Texp_record (pldes,None)   -> List.map ~f:thd3 pldes
  | Texp_field (ea,_,_)        -> [ea]
  | Texp_setfield (ea,_,_,eb)  -> [ea;eb]
  | _ -> assert false

let exp_open_env = function
  | Typedtree.Texp_open (_,_,_,env) -> env
  | _ -> assert false

let extract_functor_arg m = m

let extract_modtype_declaration m = m.Types.mtd_type
let extract_module_declaration m = m.Types.md_type

let lookup_module name env =
  let path = Env.lookup_module ~load:true name env in
  let md = Env.find_module path env in
  path, extract_module_declaration md

let tstr_eval_expression = function
  | Typedtree.Tstr_eval (e,_) -> e
  | _ -> assert false

let summary_prev =
  let open Env in
  function
  | Env_empty -> None
  | Env_open (s,_) | Env_value (s,_,_)
  | Env_type (s,_,_) | Env_extension (s,_,_)
  | Env_module (s,_,_) | Env_modtype (s,_,_)
  | Env_class (s,_,_) | Env_cltype (s,_,_)
  | Env_functor_arg (s,_) ->
    Some s

let signature_of_summary =
  let open Env in
  let open Types in
  function
  | Env_value (_,i,v)      -> Some (Sig_value (i,v))
  (* Trec_not == bluff, FIXME *)
  | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
  (* Texp_first == bluff, FIXME *)
  | Env_extension (_,i,e)  ->
    begin match e.ext_type_path with
    | Path.Pident id when Ident.name id = "exn" ->
      Some (Sig_typext (i,e, Text_exception))
    | _ ->
      Some (Sig_typext (i,e, Text_first))
    end
  | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
  | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
  | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
  | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
  | Env_open _ | Env_empty | Env_functor_arg _ -> None
