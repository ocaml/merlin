let (>>=) a f = match a with
  | Some a' -> f a'
  | None -> None

let to_linecol pos = Lexing.(pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let compare_loc pos loc =
  let open Location in
  if pos < to_linecol loc.loc_start
  then -1
  else if pos > to_linecol loc.loc_end
  then 1
  else 0

let union_loc a b =
  let open Location in
  let loc_start = 
    if to_linecol a.loc_start <= to_linecol b.loc_start
    then a.loc_start
    else b.loc_start
  and loc_end =
    if to_linecol a.loc_end <= to_linecol b.loc_end
    then b.loc_end
    else a.loc_end
  in
  { loc_start ; loc_end ; loc_ghost = a.loc_ghost && b.loc_ghost }

let union_loc_opt a b = match a,b with
  | None, None -> None
  | (Some _ as l), None | None, (Some _ as l) -> l
  | Some a, Some b -> Some (union_loc a b)

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
  in function
  | Sig_value (_,v)     -> Some v.val_loc
  | Sig_type (_,t,_)      -> Some t.type_loc
  | Sig_exception (_,e) -> Some e.exn_loc
  | Sig_modtype (_,Modtype_manifest m)
  | Sig_module (_,m,_)    -> mod_loc m
  | Sig_modtype (_,Modtype_abstract) -> None
  | Sig_class (i,c,_) -> failwith "TODO: handling classes"
  | Sig_class_type (i,c,_) -> failwith "TODO: handling classes"

let signature_ident = 
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_exception (i,_)
  | Sig_modtype (i,_)
  | Sig_module (i,_,_)
  | Sig_modtype (i,_)
  | Sig_class (i,_,_)
  | Sig_class_type (i,_,_) -> i

module Env =
struct
  let summary_prev =
    let open Env in function
    | Env_empty -> None 
    | Env_open (s,_) | Env_value (s,_,_)
    | Env_type (s,_,_) | Env_exception (s,_,_)
    | Env_module (s,_,_) | Env_modtype (s,_,_)
    | Env_class (s,_,_) | Env_cltype (s,_,_) ->
      Some s
  
  let signature_of_summary =
    let open Env in
    let open Types in function
    | Env_value (_,i,v)      -> Some (Sig_value (i,v))
    | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
    | Env_exception (_,i,e)  -> Some (Sig_exception (i,e))
    | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
    | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
    | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
    | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
    | Env_open _ | Env_empty -> None 
  
  
  let summary_at line col sum =
    let cmp = compare_loc (line,col) in
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
    let initial_summary = Env.summary (Lazy.force Typer.initial_env) in
    let rec aux summary =
      if summary == initial_summary
      then ()
      else
      match summary with
      | Env.Env_empty -> ()
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
end

module Typedtree =
struct

end
