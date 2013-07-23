(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

let (>>=) a f = match a with
  | Some a' -> f a'
  | None -> None

let union_loc_opt a b = match a,b with
  | None, None -> None
  | (Some _ as l), None | None, (Some _ as l) -> l
  | Some a, Some b -> Some (Merlin_parsing.union a b)

let rec signature_loc =
  let open Types in
  let rec mod_loc = function
    | Mty_ident _ -> None
    | Mty_functor (_,m1,m2) ->
        union_loc_opt (mod_loc m1) (mod_loc m2)
    | Mty_signature s ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
                        | (Some _ as v) -> v
                        | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
  in
  function
  | Sig_value (_,v)     -> Some v.val_loc
  | Sig_type (_,t,_)      -> Some t.type_loc
  | Sig_exception (_,e) -> Some e.exn_loc
  | Sig_modtype (_,Modtype_manifest m)
  | Sig_module (_,m,_)    -> mod_loc m
  | Sig_modtype (_,Modtype_abstract) -> None
  | Sig_class (_,_,_)
  | Sig_class_type (_,_,_) -> None

let signature_ident =
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_exception (i,_)
  | Sig_modtype (i,_)
  | Sig_module (i,_,_)
  | Sig_class (i,_,_)
  | Sig_class_type (i,_,_) -> i

let print_constructor ppf c =
  let open Types in
  match c.cstr_args with
  | [] ->
    Printtyp.type_expr ppf ({ level = 0 ; id = 0 ; desc = c.cstr_res.desc })
  | args ->
    let desc = Tarrow ("",{ level = 0; id = 0; desc = Ttuple args}, c.cstr_res,Cok) in
    Printtyp.type_expr ppf ({ level = 0 ; id = 0 ; desc  })

let summary_prev =
  let open Env in
  function
  | Env_empty -> None
  | Env_open (s,_) | Env_value (s,_,_)
  | Env_type (s,_,_) | Env_exception (s,_,_)
  | Env_module (s,_,_) | Env_modtype (s,_,_)
  | Env_class (s,_,_) | Env_cltype (s,_,_) ->
    Some s

let signature_of_summary =
  let open Env in
  let open Types in
  function
  | Env_value (_,i,v)      -> Some (Sig_value (i,v))
  | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
  | Env_exception (_,i,e)  -> Some (Sig_exception (i,e))
  | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
  | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
  | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
  | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
  | Env_open _ | Env_empty -> None

let summary_at pos sum =
  let cmp = Merlin_parsing.compare_pos pos in
  let rec aux sum =
    match signature_of_summary sum >>= signature_loc with
      | None -> summary_prev sum >>= aux
      | Some loc ->
    match cmp loc with
      | x when x < 0 -> None
      | 0 -> Some sum
      | x -> summary_prev sum >>= aux
  in
  aux sum

let signature_of_env env =
  let open Types in
  let sg = ref [] in
  let append item = sg := item :: !sg in
  let rec aux summary =
    match summary with
    | Env.Env_empty -> ()
    (* Stop when encoutering extensions *)
    | Env.Env_module (_,i,_) when i = Extensions_utils.ident -> ()
    | Env.Env_value (s,i,v) ->
        append (Sig_value (i,v));
        aux s
    | Env.Env_type (s,i,t) ->
        append (Sig_type (i,t,Trec_not)); (* Trec_not == bluff, FIXME *)
        aux s
    | Env.Env_exception (s,i,e) ->
        append (Sig_exception (i,e));
        aux s
    | Env.Env_module (s,i,m) ->
        append (Sig_module (i,m,Trec_not));
        aux s
    | Env.Env_modtype (s,i,mt) ->
        append (Sig_modtype (i,mt));
        aux s
    | Env.Env_class (s,i,c) ->
        append (Sig_class (i,c,Trec_not));
        aux s
    | Env.Env_cltype (s,i,ct) ->
        append (Sig_class_type (i,ct,Trec_not));
        aux s
    | Env.Env_open (s,p) ->
        aux s
  in
  let summary = Env.summary env in
  aux summary;
  Typemod.simplify_signature (!sg)

let rec dump_ts ts =
  let dump_t { Browse. loc ; context ; nodes = lazy nodes } =
    let kind = match context with
      | Browse.Type _ -> "type"
      | Browse.TypeDecl _ -> "type_decl"
      | Browse.Expr _ -> "expr"
      | Browse.Pattern _ -> "pattern"
      | Browse.Module _ -> "module"
      | Browse.Modtype _ -> "modtype"
      | Browse.Class (_, _) -> "class"
      | Browse.ClassType _ -> "class_type"
      | Browse.MethodCall _ -> "#"
      | Browse.Other -> "other"
    in
    Protocol.with_location loc
    [
      "kind", `String kind;
      "children", dump_ts nodes
    ]
  in
  let cmp_start { Browse.loc = l1 } { Browse.loc = l2 } =
    Misc.compare_pos l1.Location.loc_start l2.Location.loc_end
  in
  let ts = List.sort cmp_start ts in
  `List (List.map dump_t ts)
