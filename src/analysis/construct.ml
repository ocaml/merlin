(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Arthur Wendling  <art.wendling(_)gmail.com>

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

open Std
open BrowseT
open Types
open Parsetree
module A = Ast_helper

let section = Logger.Section.of_string "construct"

exception Not_allowed of string
exception Too_many

let () =
  Location.register_error_of_exn (function
    | Not_allowed s  -> Some (Location.error ("Construct not allowed on " ^ s))
    | _ -> None
  )

let mk_id s  = Location.mknoloc (Longident.Lident s)
let mk_var s = Location.mknoloc s
let id_of_path path = mk_var (Untypeast.lident_of_path path)
let var_of_id id = mk_var id.Ident.name

module Predef_types = struct
  let tbl = Hashtbl.create 14

  let () =
    let mk s = A.Exp.construct (mk_id s) None in
    List.iter ~f:(fun (k, v) -> Hashtbl.add tbl k v) [
      Predef.path_int, mk "0" ;
      Predef.path_char, mk "'c'" ;
      Predef.path_string, mk "\"\"" ;
      Predef.path_float, mk "0.0" ;
      Predef.path_bool, mk "true" ;
      Predef.path_unit, mk "()" ;
      Predef.path_exn, mk "exn" ;
      Predef.path_array, mk "[| |]" ;
      Predef.path_list, mk "[ ]" ;
      (* The extra space is used to avoid the sugar [_a] in (_a :: []) *)
      Predef.path_option, mk "None" ;
      Predef.path_nativeint, mk "0n" ;
      Predef.path_int32, mk "0l" ;
      Predef.path_int64, mk "0L" ;
      Predef.path_lazy_t, mk "(lazy)" ;
    ]
end

let bind name t env =
  let vd = {
    val_type = t ;
    val_kind = Val_unbound ;
    val_loc = Location.none ;
    val_attributes = []
  } in
  Env.add_value (Ident.create name) vd env

let already_defined name env =
  try let _ = Env.lookup_value (Longident.Lident name) env in true
  with Not_found -> false

let freevar ?(prefix = "") t env =
  let rec to_string i =
    let n, m = i mod 26, i / 26 in
    let s = String.make 1 (Char.chr (Char.code 'a' + n)) in
    if m = 0
    then s
    else to_string (m - 1) ^ s in
  let rec go i =
    let name = prefix ^ to_string i in
    if already_defined name env
    then go (i + 1)
    else name in
  let name = go 0 in
  name, bind name t env

let rec hole type_expr env =
  let type_expr = Btype.repr type_expr in
  try match type_expr.desc with
  | Tconstr (path, _, _) -> Hashtbl.find Predef_types.tbl path, env
  | _ -> raise Not_found
  with Not_found ->
    let name, env' = freevar ~prefix:"_" type_expr env in
    let v = A.Exp.ident (mk_id name) in
    v, env'

let prefix env path =
  let path = Printtyp.shorten_path ~env path in
  match Path.to_string_list path with
  | [] -> assert false
  | p :: ps ->
    fun name ->
      let open Longident in
      match
        List.fold_left ps ~init:(Lident p) ~f:(fun lid p -> Ldot (lid, p))
      with
      | Lident _ -> Lident name
      | Ldot (lid, _) -> Ldot (lid, name)
      | _ -> assert false

let map_signature f items =
  List.filter_map items
    ~f:(function
      | Sig_type (id, _, _)
        when id.Ident.name.[0] = '#'
        (* type #class = < .. > *)
      -> None
      | sig_item ->
        Some (f sig_item))

let ( >>= ) xs f = List.concat_map ~f xs
let too_many type_expr env = [ hole type_expr env ]

let counting ~many env type_expr f =
  let count = ref 0 in
  try f ~many:(fun () -> many && !count = 0)
        ~ret:(fun xs ->
                count := !count + List.length xs ;
                if not many && !count > 1 then raise Too_many ;
                xs)
  with Too_many ->
    [ hole type_expr env ]

(* Stop constructing when max_depth < 0 *)
let max_depth = ref (-1)

let rec gen_expr1 env type_expr =
  match gen_expr ~many:false env type_expr with
  | [] -> raise (Not_allowed "no results") (* impossible *)
  | [res] -> res
  | _ -> assert false

and gen_expr ~many env type_expr =
  decr max_depth ;
  let res =
    if !max_depth < 0
    then [ hole type_expr env ]
    else gen_expr' ~many env type_expr in
  incr max_depth ;
  res

and gen_expr' ~many env type_expr =
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tunivar _ | Tvar _ -> [ hole type_expr env ]
  | Tsubst t -> gen_expr ~many env t
  | Tpoly (t, _) -> gen_expr ~many env t

  | Tconstr (path, params, _) ->
    let def = try Env.find_type_descrs path env with Not_found -> [], [] in
    begin match def with
    | [], [] -> from_type_decl ~many env path params type_expr
    | [], labels -> gen_record ~many env path labels type_expr
    | constrs, [] -> gen_constrs ~many env path constrs type_expr
    | _ -> assert false
    end

  | Tvariant row_desc ->
    gen_variant ~many env row_desc type_expr

  | Ttuple ts ->
    gen_tuple ~many env ts

  | Tarrow (label, t0, t1, _) ->
    let lbl =
      if label <> "" && label.[0] = '?'
      then String.sub label 1 (String.length label - 1)
      else label in
    let name, env' =
      if lbl = "" || already_defined lbl env
      then freevar t0 env
      else lbl, bind lbl t0 env in
    gen_expr ~many env' t1 >>= fun (out, env'') ->
    let ast =
      A.Exp.fun_ label None
        (A.Pat.var (mk_var name))
        out in
    [ ast, env'' ]

  | Tpackage (path, ids, args) ->
    begin try
      let ty = Typemod.modtype_of_package env Location.none path ids args in
      let ast =
        A.Exp.constraint_
          (A.Exp.pack (gen_module env ty))
          (gen_core_type type_expr) in
      [ ast, env ]
    with Typemod.Error _ -> raise (Not_allowed "first-class module")
    end

  | Tobject (fields, _) ->
    let rec go acc type_expr =
       let type_expr = Btype.repr type_expr in
       match type_expr.desc with
       | Tnil | Tvar None -> List.rev acc
       | Tfield (name, kind, ty, tail) ->
         let expr, _ = gen_expr1 env ty in
         let field =
           A.Cf.method_ (mk_var name) Asttypes.Public
             (A.Cf.concrete Asttypes.Fresh expr) in
         go (field :: acc) tail
       | _ ->
          Logger.errorf section (fun fmt () ->
            Format.fprintf fmt "object type ends in %a"
              Printtyp.type_expr type_expr
          ) () ;
         assert false in
    let fields = go [] fields in
    let ast =
      A.Exp.object_
        (A.Cstr.mk
           (A.Pat.any ())
           fields) in
    [ ast, env ]
  | Tfield _ -> raise (Not_allowed "field")
  | Tnil -> raise (Not_allowed "nil")

and from_type_decl ~many env path params texpr =
  try let tdecl = Ctype.instance_declaration (Env.find_type path env) in
      match tdecl.type_manifest with
      | Some te ->
        List.iter2 ~f:(Ctype.unify env) params tdecl.type_params ;
        gen_expr' ~many env te
      | None -> raise Not_found
  with Not_found -> [ hole texpr env ]

and gen_product ~many env types =
  let rec go ~many acc env = function
    | [] -> [ List.rev acc, env ]
    | t::ts ->
      let es = gen_expr ~many env t in
      let many = many && List.length es <= 1 in
      es >>= fun (h, env') ->
      go ~many (h::acc) env' ts in
  go ~many [] env types

and gen_tuple ~many env types =
  gen_product ~many env types >>= fun (holes, env') ->
  [ A.Exp.tuple holes, env' ]

and gen_record ~many env path labels type_expr =
  let types =
    List.map labels ~f:(fun lbl ->
        let _, arg, res = Ctype.instance_label true lbl in
        Ctype.unify env res type_expr ;
        arg) in
  gen_product ~many env types >>= fun (holes, env') ->
  let is_first = ref true in
  let fields =
    List.map2 labels holes ~f:(fun lbl expr ->
      let field =
        if !is_first (* add a "Module." to the first record field *)
        then mk_var (prefix env path lbl.lbl_name)
        else mk_id lbl.lbl_name in
      is_first := false ;
      field, expr) in
  [ A.Exp.record fields None, env' ]

and gen_constrs ~many env path constrs type_expr =
  let are_types_unifiable cstr_descr =
    let snap = Btype.snapshot () in
    let _, ty_res = Ctype.instance_constructor cstr_descr in
    let res =
      try Ctype.unify_gadt ~newtype_level:0 (ref env) type_expr ty_res ; true
      with Ctype.Unify _ -> false in
    Btype.backtrack snap ;
    res in
  let constrs =
    List.filter_map constrs ~f:(fun cstr_descr ->
      if cstr_descr.cstr_generalized
         && not (are_types_unifiable cstr_descr)
      then None
      else Some cstr_descr) in
  match constrs with
  | [] -> raise (Not_allowed "no constructor")
  | cstr_descrs ->
    counting ~many env type_expr begin fun ~many ~ret ->
      cstr_descrs >>= fun cstr_descr ->
      (if cstr_descr.cstr_arity <= 0
       then ret [ None, env ]
       else begin
         let snap = Btype.snapshot () in
         let r_env = ref env in
         let ty_args, ty_res = Ctype.instance_constructor cstr_descr in
         let res =
           try Ctype.unify_gadt ~newtype_level:0 r_env type_expr ty_res ;
               let env = !r_env in
               gen_tuple ~many:(many ()) env ty_args >>= fun (t, env') ->
               [ Some t, env' ]
           with Ctype.Unify _ | Not_allowed _ -> [] in
         Btype.backtrack snap ;
         ret res
       end)
      >>= fun (args, env') ->
      let lidl = mk_var (prefix env path cstr_descr.cstr_name) in
      [ A.Exp.construct lidl args, env ]
    end

and gen_variant ~many env row_desc type_expr =
  let fields =
    List.filter
      (fun (lbl, row_field) -> match row_field with
         | Rpresent _
         | Reither (true, [], _, _)
         | Reither (false, [_], _, _) -> true
         | _ -> false)
      row_desc.row_fields in
  match fields with
  | [] -> raise (Not_allowed "empty variant type")
  | row_descrs ->
    counting ~many env type_expr begin fun ~many ~ret ->
      List.rev row_descrs >>= fun (lbl, row_field) ->
      (match row_field with
       | Reither (false, [ty], _, _) | Rpresent (Some ty) ->
         (try gen_expr ~many:(many ()) env ty >>= fun (expr, env') ->
              ret [ Some expr, env' ]
          with Not_allowed _ -> [ ])
       | _ ->
         ret [ None, env ])
       >>= fun (arg, env') ->
       [ A.Exp.variant lbl arg, env' ]
    end

and gen_module env mod_type =
  match mod_type with
  | Mty_signature lazy_sig ->
    let sg = Lazy.force lazy_sig in
    let items = map_signature (gen_signature_item env) sg in
    A.Mod.structure items
   | Mty_ident path ->
    (try let m = Env.find_modtype path env in
         match m.mtd_type with
         | Some t -> gen_module env t
         | None -> raise Not_found
     with Not_found -> raise (Not_allowed "module ident"))
   | Mty_alias path ->
    (try let m = Env.find_module path env in
         gen_module env m.md_type
     with Not_found -> raise (Not_allowed "module alias"))
  | Mty_functor (id, arg, out) ->
    A.Mod.functor_ (var_of_id id)
      (Option.map gen_modtype arg)
      (gen_module env out)

and gen_modtype module_type =
  match module_type with
  | Mty_ident path -> A.Mty.ident (id_of_path path)
  | Mty_alias path -> A.Mty.alias (id_of_path path)
  | Mty_signature s ->
    let s = Lazy.force s in
    A.Mty.signature (map_signature gen_sig_item s)
  | Mty_functor (id, arg, out) ->
    A.Mty.functor_ (var_of_id id)
      (Option.map gen_modtype arg)
      (gen_modtype out)

and gen_core_type type_expr =
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tsubst e -> gen_core_type e
  | Tunivar None | Tvar None -> A.Typ.any ()
  | Tunivar (Some name) | Tvar (Some name) -> A.Typ.var name
  | Ttuple ts ->
    A.Typ.tuple (List.map gen_core_type ts)
  | Tpoly (t, vs) ->
    A.Typ.poly
      (List.map
         (fun v -> match v.desc with
           | Tunivar (Some name) | Tvar (Some name) -> name
           | _ -> failwith "poly: not a var")
          vs)
      (gen_core_type t)
  | Tconstr (path, params, _) ->
    A.Typ.constr (id_of_path path)
      (List.map gen_core_type params)
  | Tarrow (label, t0, t1, _) ->
    A.Typ.arrow label (gen_core_type t0) (gen_core_type t1)
  | Tpackage (path, lids, args) ->
    A.Typ.package (id_of_path path)
      (List.map2 lids args
         ~f:(fun id t -> (mk_var id), gen_core_type t))
  | Tvariant row_desc ->
    A.Typ.variant
      (List.map row_desc.row_fields
         ~f:(fun (lbl, row_field) -> match row_field with
            | Rpresent None ->
              Rtag (lbl, [], true, [])
            | Rpresent (Some t) ->
              Rtag (lbl, [], false, [gen_core_type t])
            | Reither (is_empty, ts, _, _) ->
              Rtag (lbl, [], is_empty, List.map gen_core_type ts)
            | Rabsent -> assert false))
      (if row_desc.row_closed then Asttypes.Closed else Asttypes.Open)
      None
  | Tobject (fields, _) ->
    let rec go acc type_expr =
       let type_expr = Btype.repr type_expr in
       match type_expr.desc with
       | Tnil      -> List.rev acc, Asttypes.Closed
       | Tvar None -> List.rev acc, Asttypes.Open
       | Tfield ("*dummy method*", _, _, tail) ->
         go acc tail
       | Tfield (name, kind, def, tail) ->
         let field = (name, [], gen_core_type def) in
         go (field :: acc) tail
       | other ->
          Logger.errorf section (fun fmt () ->
            Format.fprintf fmt "object type ends in %a"
              Printtyp.type_expr type_expr
          ) () ;
         assert false in
    let fields, closed = go [] fields in
    A.Typ.object_ fields closed
  | Tfield _ -> raise (Not_allowed "field")
  | Tnil -> raise (Not_allowed "nil")

and gen_sig_value id vd =
  { pval_name = var_of_id id
  ; pval_type = gen_core_type vd.val_type
  ; pval_prim = []
  ; pval_attributes = vd.val_attributes
  ; pval_loc = Location.none
  }

and gen_type_decl id type_decl =
  [ A.Type.mk
      ?manifest:
        (match type_decl.type_manifest with
         | None -> None
         | Some m -> Some (gen_core_type m))
      ~params:
        (List.map
           (fun t ->
             let ct = gen_core_type t in
             ct, Asttypes.Invariant)
           type_decl.type_params)
      ~kind:
        (match type_decl.type_kind with
         | Type_open -> Ptype_open
         | Type_abstract -> Ptype_abstract
         | Type_record (labels, _) ->
           Ptype_record
             (List.map (fun lbl ->
                  { pld_name = var_of_id lbl.ld_id
                  ; pld_mutable = lbl.ld_mutable
                  ; pld_type = gen_core_type lbl.ld_type
                  ; pld_loc = Location.none
                  ; pld_attributes = lbl.ld_attributes
                  })
                 labels)
         | Type_variant cstrs ->
           Ptype_variant
             (List.map (fun c ->
                  { pcd_name = var_of_id c.cd_id
                  ; pcd_args = List.map gen_core_type c.cd_args
                  ; pcd_res =
                      (match c.cd_res with
                       | None -> None
                       | Some t -> Some (gen_core_type t))
                  ; pcd_loc = Location.none
                  ; pcd_attributes = c.cd_attributes
                  })
                 cstrs))
      (var_of_id id) ]

and gen_extension_constructor id ext =
  { pext_name = var_of_id id
  ; pext_kind =
      Pext_decl
        (List.map gen_core_type ext.ext_args,
         Option.map gen_core_type ext.ext_ret_type)
  ; pext_loc = Location.none
  ; pext_attributes = ext.ext_attributes
  }

and gen_type_extension id ext =
  A.Te.mk (mk_id (Path.name ext.ext_type_path))
    [ gen_extension_constructor id ext ]

and gen_modtype_decl id modtype_decl =
  A.Mtd.mk
    ?typ:
      (match modtype_decl.mtd_type with
       | None -> None
       | Some t -> Some (gen_modtype t))
    (var_of_id id)

and gen_class_descr params path type_ =
  gen_class_infos params path (gen_class_type type_)

and gen_class_decl env params path type_ =
  gen_class_infos params path (gen_class_expr env type_)

and gen_class_infos
  : type a. type_expr list -> Path.t -> a -> a class_infos
  = fun params path expr ->
  { pci_virt = Asttypes.Concrete
  ; pci_params = List.map (fun t -> gen_core_type t, Asttypes.Invariant) params
  ; pci_name = mk_var (Path.last path)
  ; pci_expr = expr
  ; pci_loc = Location.none
  ; pci_attributes = []
  }

and gen_class_type cty =
  { pcty_loc = Location.none
  ; pcty_attributes = []
  ; pcty_desc =
      match cty with
      | Cty_constr (path, args, cty) ->
        Pcty_constr
          (id_of_path path,
           List.map gen_core_type args)
      | Cty_signature class_sig ->
        Pcty_signature
          { pcsig_self = gen_core_type class_sig.csig_self
          ; pcsig_fields = [] (* todo *)
          }
      | Cty_arrow (lbl, ty, cty) ->
        Pcty_arrow (lbl, gen_core_type ty, gen_class_type cty)
  }

and gen_class_expr env cty =
  { pcl_loc = Location.none
  ; pcl_attributes = []
  ; pcl_desc =
      match cty with
      | Cty_constr (path, args, cty) ->
        Pcl_constr (id_of_path path,
                    List.map gen_core_type args)
      | Cty_arrow (lbl, ty, cty) ->
        let var, env = freevar ty env in
        let var = A.Pat.var (mk_var var) in
        Pcl_fun (lbl, None, var, gen_class_expr env cty)
      | Cty_signature class_sig ->
        let type_expr = Btype.repr class_sig.csig_self in
        begin match type_expr.desc with
          | Tobject (fields, _) ->
            let rec go acc type_expr =
               let type_expr = Btype.repr type_expr in
               match type_expr.desc with
               | Tnil | Tvar None -> List.rev acc
               | Tfield ("*dummy method*", _, _, tail) ->
                 go acc tail
               | Tfield (name, kind, def, tail) ->
                 let expr, _ = gen_expr1 env def in
                 let field =
                   gen_class_field
                     (Pcf_method
                        (mk_var name,
                         Asttypes.Public,
                         Cfk_concrete (Asttypes.Fresh, expr))) in

                 go (field :: acc) tail
               | other ->
                  Logger.errorf section (fun fmt () ->
                    Format.fprintf fmt "object type ends in %a"
                      Printtyp.type_expr type_expr
                  ) () ;
                 assert false in
            let methods = go [] fields in
            let vars =
              Vars.fold
                (fun name (is_mutable, is_virtual, ty) lst ->
                   let def =
                     match is_virtual with
                     | Asttypes.Virtual ->
                       Cfk_virtual (gen_core_type ty)
                     | Asttypes.Concrete ->
                       let expr, _ = gen_expr1 env ty in
                       Cfk_concrete (Asttypes.Fresh, expr) in
                   let var =
                     gen_class_field
                       (Pcf_val (mk_var name, is_mutable, def)) in
                   var :: lst)
                class_sig.csig_vars
                [] in
            Pcl_structure
              { pcstr_self = A.Pat.any ()
              ; pcstr_fields = vars @ methods
              }
          | _ -> failwith "not an object"
        end
  }

and gen_class_field f =
  { pcf_loc = Location.none
  ; pcf_attributes = []
  ; pcf_desc = f
  }

and gen_sig_item sig_item =
  match sig_item with
  | Sig_value (id, vd) ->
    A.Sig.value (gen_sig_value id vd)
  | Sig_type (id, type_decl, _) ->
    A.Sig.type_ (gen_type_decl id type_decl)
  | Sig_typext (id, ext, Text_exception) ->
    A.Sig.exception_ (gen_extension_constructor id ext)
  | Sig_typext (id, ext, _) ->
    A.Sig.type_extension (gen_type_extension id ext)
  | Sig_modtype (id, modtype_decl) ->
    A.Sig.modtype (gen_modtype_decl id modtype_decl)
  | Sig_module (id, mod_decl, _) ->
    A.Sig.module_
      (A.Md.mk (var_of_id id)
         (gen_modtype mod_decl.md_type))
  | Sig_class (_, c, _) ->
    A.Sig.class_
      [ gen_class_descr c.cty_params c.cty_path c.cty_type ]
  | Sig_class_type (_, c, _) ->
    A.Sig.class_type
      [ gen_class_descr c.clty_params c.clty_path c.clty_type ]

and gen_signature_item env sig_item =
  match sig_item with
  | Sig_value (id, vd) ->
    let expr, _ = gen_expr1 env vd.val_type in
    A.Str.value
      Asttypes.Nonrecursive
      [ A.Vb.mk (A.Pat.var (var_of_id id)) expr ]
  | Sig_type (id, type_decl, _) ->
    A.Str.type_ (gen_type_decl id type_decl)
  | Sig_typext (id, ext, Text_exception) ->
    A.Str.exception_ (gen_extension_constructor id ext)
  | Sig_typext (id, ext, _) ->
    A.Str.type_extension (gen_type_extension id ext)
  | Sig_modtype (id, modtype_decl) ->
    A.Str.modtype (gen_modtype_decl id modtype_decl)
  | Sig_module (id, mod_decl, _) ->
    A.Str.module_
      (A.Mb.mk (var_of_id id)
         (gen_module env mod_decl.md_type))
  | Sig_class (_, c, _) ->
    A.Str.class_
      [ gen_class_decl env c.cty_params c.cty_path c.cty_type ]
  | Sig_class_type (_, c, _) ->
    A.Str.class_type
      [ gen_class_descr c.clty_params c.clty_path c.clty_type ]


let needs_parentheses e = match e.pexp_desc with
  | Pexp_fun _
  | Pexp_construct (_, Some _)
  -> true
  | _ -> false

let string_of_format ~parens print e =
  let fmt, to_string = Format.to_string () in
  print fmt e ;
  let str = to_string () in
  if parens then "(" ^ str ^ ")" else str

let string_of_expr ~parens e =
  string_of_format ~parens Pprintast.default#expression e
let string_of_module m =
  string_of_format ~parens:false Pprintast.default#module_expr m

let node ~max_depth:d ~loc ~env parents node =
  max_depth := d ;
  match node.t_node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let strs =
      gen_expr ~many:true env ty >>= fun (e, _) ->
      [ string_of_expr ~parens:(needs_parentheses e) e ] in
    loc, strs
  | Module_expr expr ->
    let ty = expr.Typedtree.mod_type in
    let result = gen_module env ty in
    loc, [ string_of_module result ]
  | node ->
    raise (Not_allowed (BrowseT.string_of_node node))


(* Apply: construct arguments *)

let rec argument_types env type_expr =
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tarrow (label, t0, t1, _) -> (label, t0) :: argument_types env t1
  | Tconstr (path, params, _) ->
    (try let tdecl = Ctype.instance_declaration (Env.find_type path env) in
         match tdecl.type_manifest with
         | Some te ->
           List.iter2 ~f:(Ctype.unify env) params tdecl.type_params ;
           argument_types env te
         | None -> raise Not_found
     with Ctype.Unify _ | Not_found -> [])
  | _ -> []

let rec functor_argument_types env mod_type =
  try match mod_type with
  | Mty_signature _ -> []
  | Mty_functor (id, arg, out) ->
    (id, arg) :: functor_argument_types env out
  | Mty_alias path ->
     let m = Env.find_module path env in
     functor_argument_types env m.md_type
  | Mty_ident path ->
    let m = Env.find_modtype path env in
    match m.mtd_type with
    | Some t -> functor_argument_types env t
    | None -> []
  with Not_found -> []

let apply ~max_depth:d ~loc ~env parents node =
  max_depth := d - 1 ;
  let loc = { loc with Location.loc_start = loc.Location.loc_end } in
  match node.t_node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let args = argument_types env ty in
    let labels, types = List.split args in
    let it = Untypeast.untype_expression expr in
    let strs =
      gen_product ~many:true env types >>= fun (exprs, _) ->
      let args = List.combine labels exprs in
      let result = A.Exp.apply (A.Exp.ident (mk_id "_")) args in
      let str = string_of_expr ~parens:false result in
      [ String.sub str ~pos:1 ~len:(String.length str - 1) ] in
    loc, strs
  | Module_expr expr ->
    let ty = expr.Typedtree.mod_type in
    let args = functor_argument_types env ty in
    let ids, modtypes = List.split args in
    let modules =
      List.map2 ids modtypes ~f:(fun id mty ->
          match mty with
          | None -> (* no module type available *)
            A.Mod.ident (mk_id id.Ident.name)
          | Some modtype ->
            if !max_depth > 0
            then gen_module env modtype
            else A.Mod.constraint_ (* module type hole *)
                   (A.Mod.unpack (A.Exp.ident (mk_id ("_" ^ id.Ident.name))))
                   (gen_modtype modtype)) in
    let result =
      List.fold_left modules ~init:(A.Mod.ident (mk_id " "))
        ~f:(fun acc m -> A.Mod.apply acc m) in
    loc, [ string_of_module result ]
  | node ->
    raise (Not_allowed (BrowseT.string_of_node node))
