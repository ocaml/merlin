open Std

module P = struct
  open Raw_parser

  type st = unit
  type t = Env.t * Typedtree.structure list

  let empty () =
    let env = Env.initial in
    let env = Env.open_pers_signature "Pervasives" env in
    env, []

  let rec find_structure md =
    match md.Typedtree.mod_desc with
    | Typedtree.Tmod_structure _ -> Some md
    | Typedtree.Tmod_functor (_,_,_,md) -> find_structure md
    | Typedtree.Tmod_constraint (md,_,_,_) -> Some md
    | _ -> None

  let frame () f (env,structures) =
    let mkeval e = {
      Parsetree. pstr_desc = Parsetree.Pstr_eval e;
      pstr_loc = e.Parsetree.pexp_loc;
    } in
    let case =
      match Merlin_parser.value f with
      | Terminal _ | Bottom -> `none
      | Nonterminal nt ->
      match nt with
      | NT'structure str | NT'structure_tail str | NT'structure_item str ->
        `str str
      | NT'top_expr e | NT'strict_binding e | NT'simple_expr e | NT'seq_expr e
      | NT'opt_default (Some e) | NT'fun_def e | NT'fun_binding e | NT'expr e
      | NT'match_action e | NT'labeled_simple_expr (_,e) | NT'label_ident (_,e)
      | NT'label_expr (_,e) ->
        `str [mkeval e]
      | NT'expr_semi_list el | NT'expr_comma_opt_list el
      | NT'expr_comma_list el  ->
        `str (List.map ~f:mkeval el)
      | NT'module_expr pmod | NT'module_binding pmod ->
        `md pmod
      | NT'signature_item sg ->
        `sg sg
      | NT'signature sg ->
        `sg (List.rev sg)
      | NT'module_functor_arg (id,mty) ->
        `fmd (id,mty)
      | _ -> `none
    in
    match case with
    | `str str ->
      let structures',_,env = Typemod.type_structure env str Location.none in
      (env, structures' :: structures)
    | `sg sg ->
      let sg = Typemod.transl_signature env sg in
      let sg = sg.Typedtree.sig_type in
      Env.add_signature sg env, structures
    | `md pmod ->
      let tymod = Typemod.type_module env pmod in
      begin match find_structure tymod with
        | None -> env
        | Some md -> md.Typedtree.mod_env
      end, structures
    | `fmd (id,mty) ->
      let mexpr = Parsetree.Pmod_structure [] in
      let mexpr = { Parsetree. pmod_desc = mexpr; pmod_loc = Location.none } in
      let mexpr = Parsetree.Pmod_functor (id, mty, mexpr) in
      let mexpr = { Parsetree. pmod_desc = mexpr; pmod_loc = Location.none } in
      let tymod = Typemod.type_module env mexpr in
      begin match find_structure tymod with
        | None -> env
        | Some md -> md.Typedtree.mod_env
      end, structures
    | `none -> (env, structures)

  let delta () f t ~old:_ = frame () f t

  let validate () _ = true
end

include Merlin_parser.Integrate (P)
