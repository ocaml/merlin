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

let section = Logger.Section.of_string "construct"

exception Not_allowed of string

let () =
  Location.register_error_of_exn (function
    | Not_allowed s  -> Some (Location.error ("Construct not allowed on " ^ s))
    | _ -> None
  )

let mk_id s  = Location.mknoloc (Longident.Lident s)
let mk_var s = Location.mknoloc s

module Predef_types = struct
  let unit_ () =
    Ast_helper.Exp.construct (mk_var (Longident.Lident "()")) None

  let char_ () =
    Ast_helper.Exp.constant (Asttypes.Const_char 'c')

  let int_ () =
    Ast_helper.Exp.constant (Asttypes.Const_int 0)

  let string_ () =
    Ast_helper.Exp.constant (Ast_helper.const_string "")

  let list_ () =
    Ast_helper.Exp.construct (mk_var (Longident.Lident "[]")) None

  let array_ () =
    Ast_helper.Exp.array []

  let tbl = Hashtbl.create 6

  let () =
    List.iter ~f:(fun (k, v) -> Hashtbl.add tbl k v) [
      Predef.path_unit, unit_ ;
      Predef.path_char, char_ ;
      Predef.path_int, int_ ;
      Predef.path_string, string_ ;
      Predef.path_list, list_ ;
      Predef.path_array, array_ ;
    ]
end

let bind name t env =
  let vd = {
    Types.val_type = t ;
    val_kind = Types.Val_unbound ;
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

let hole t env =
  let name, env' = freevar ~prefix:"_" t env in
  let hole = Ast_helper.Exp.ident (mk_id name) in
  hole, env'

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
      | Types.Sig_type (id, _, _)
        when id.Ident.name.[0] = '#'
        (* type #class = < .. > *)
      -> None
      | sig_item ->
        Some (f sig_item))

let ( >>= ) xs f = List.concat_map ~f xs
let too_many type_expr env = [ hole type_expr env ]

let rec gen_expr1 env type_expr =
  match gen_expr ~many:false env type_expr with
  | [] -> raise (Not_allowed "no results") (* impossible *)
  | [res] -> res
  | _ -> assert false

and gen_expr ~many env type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tunivar _ | Tvar _ -> raise (Not_allowed "non-immediate type")
  | Tsubst t -> gen_expr ~many env t
  | Tpoly (t, _) -> gen_expr ~many env t

  | Tconstr (path, params, _) ->
    begin try [ Hashtbl.find Predef_types.tbl path (), env ]
    with Not_found ->
      match Env.find_type_descrs path env with
      | [], labels when labels <> [] ->
        gen_record ~many env path labels type_expr
      | constrs, [] ->
        gen_constrs ~many env path constrs type_expr
      | _ -> raise (Not_allowed "constr")
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
      Ast_helper.Exp.fun_ label None
        (Ast_helper.Pat.var (mk_var name))
        out in
    [ ast, env'' ]

  | Tpackage (path, ids, args) ->
    begin try
      let ty = Typemod.modtype_of_package env Location.none path ids args in
      let ast =
        Ast_helper.Exp.constraint_
          (Ast_helper.Exp.pack (gen_module env ty))
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
           Ast_helper.Cf.method_ (mk_var name) Asttypes.Public
             (Ast_helper.Cf.concrete Asttypes.Fresh expr) in
         go (field :: acc) tail
       | _ ->
          Logger.errorf section (fun fmt () ->
            Format.fprintf fmt "object type ends in %a"
              Printtyp.type_expr type_expr
          ) () ;
         assert false in
    let fields = go [] fields in
    let ast =
      Ast_helper.Exp.object_
        (Ast_helper.Cstr.mk
           (Ast_helper.Pat.any ())
           fields) in
    [ ast, env ]
  | Tfield _ -> raise (Not_allowed "field")
  | Tnil -> raise (Not_allowed "nil")

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
  [ Ast_helper.Exp.tuple holes, env' ]

and gen_record ~many env path labels type_expr =
  let open Types in
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
  [ Ast_helper.Exp.record fields None, env' ]

and gen_constrs ~many env path constrs type_expr =
  let open Types in
  let are_types_unifiable typ =
    let snap = Btype.snapshot () in
    let res =
      try Ctype.unify_gadt ~newtype_level:0 (ref env) type_expr typ ; true
      with Ctype.Unify _trace -> false in
    Btype.backtrack snap ;
    res in
  let constrs =
    List.filter_map constrs ~f:(fun cstr_descr ->
      if cstr_descr.cstr_generalized
         && not (are_types_unifiable cstr_descr.cstr_res)
      then None
      else Some cstr_descr) in
  match constrs with
  | [] -> raise (Not_allowed "no constructor")
  | cstr_descrs ->
    let is_single = List.length cstr_descrs = 1 in
    if not many && not is_single
    then too_many type_expr env
    else let many = many && is_single in
         cstr_descrs >>= fun cstr_descr ->
         (if cstr_descr.cstr_arity <= 0
          then [ None, env ]
          else begin
            let snap = Btype.snapshot () in
            let r_env = ref env in
            let ty_args, ty_res = Ctype.instance_constructor cstr_descr in
            Ctype.unify_gadt ~newtype_level:0 r_env type_expr ty_res ;
            let env = !r_env in
            let res =
              try gen_tuple ~many env ty_args >>= fun (t, env') ->
                  [ Some t, env' ]
              with Not_allowed _ -> [] in
            Btype.backtrack snap ;
            res
          end)
         >>= fun (args, env') ->
         let lidl = mk_var (prefix env path cstr_descr.cstr_name) in
         [ Ast_helper.Exp.construct lidl args, env ]

and gen_variant ~many env row_desc type_expr =
  let open Types in
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
    let is_single = List.length row_descrs = 1 in
    if not many && not is_single
    then too_many type_expr env
    else let many = many && is_single in
         row_descrs >>= fun (lbl, row_field) ->
         (match row_field with
           | Reither (false, [ty], _, _) | Rpresent (Some ty) ->
             (try gen_expr ~many env ty >>= fun (expr, env') ->
                 [ Some expr, env' ]
              with Not_allowed _ ->
                 [ ])
           | _ ->
             [ None, env ])
         >>= fun (arg, env') ->
         [ Ast_helper.Exp.variant lbl arg, env' ]

and gen_module env mod_type =
  let open Types in
  match mod_type with
  | Mty_signature lazy_sig ->
    let sg = Lazy.force lazy_sig in
    let items = map_signature (gen_signature_item env) sg in
    Ast_helper.Mod.structure items
  | Mty_ident path | Mty_alias path ->
    let m = Env.find_modtype path env in
    begin match m.mtd_type with
      | Some t -> gen_module env t
      | None -> raise (Not_allowed "module type")
    end
  | Mty_functor (id, arg, out) ->
    Ast_helper.Mod.functor_ (mk_var id.Ident.name)
      (Option.map gen_modtype arg)
      (gen_module env out)

and gen_modtype module_type =
  let open Types in
  match module_type with
  | Mty_ident path ->
    Ast_helper.Mty.ident (mk_var (Untypeast.lident_of_path path))
  | Mty_alias path ->
    Ast_helper.Mty.alias (mk_var (Untypeast.lident_of_path path))
  | Mty_signature s ->
    let s = Lazy.force s in
    Ast_helper.Mty.signature (map_signature gen_sig_item s)
  | Mty_functor (id, arg, out) ->
    Ast_helper.Mty.functor_ (mk_var id.Ident.name)
      (Option.map gen_modtype arg)
      (gen_modtype out)

and gen_core_type type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tsubst e -> gen_core_type e
  | Tunivar None | Tvar None -> Ast_helper.Typ.any ()
  | Tunivar (Some name) | Tvar (Some name) -> Ast_helper.Typ.var name
  | Ttuple ts ->
    Ast_helper.Typ.tuple (List.map gen_core_type ts)
  | Tpoly (t, vs) ->
    Ast_helper.Typ.poly
      (List.map
         (fun v -> match v.desc with
           | Tunivar (Some name) | Tvar (Some name) -> name
           | _ -> failwith "poly: not a var")
          vs)
      (gen_core_type t)
  | Tconstr (path, params, _) ->
    Ast_helper.Typ.constr
      (mk_var (Untypeast.lident_of_path path))
      (List.map gen_core_type params)
  | Tarrow (label, t0, t1, _) ->
    Ast_helper.Typ.arrow label (gen_core_type t0) (gen_core_type t1)
  | Tpackage (path, lids, args) ->
    Ast_helper.Typ.package
      (mk_var (Untypeast.lident_of_path path))
      (List.map2
         (fun id t -> (mk_var id), gen_core_type t)
         lids
         args)
  | Tvariant row_desc ->
    Ast_helper.Typ.variant
      (List.map
         (fun (lbl, row_field) -> match row_field with
            | Rpresent None ->
              Parsetree.Rtag (lbl, [], true, [])
            | Rpresent (Some t) ->
              Parsetree.Rtag (lbl, [], false, [gen_core_type t])
            | Reither (is_empty, ts, _, _) ->
              Parsetree.Rtag (lbl, [], is_empty, List.map gen_core_type ts)
            | Rabsent -> assert false)
         row_desc.row_fields)
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
    Ast_helper.Typ.object_ fields closed
  | Tfield _ -> raise (Not_allowed "field")
  | Tnil -> raise (Not_allowed "nil")

and gen_sig_value id vd =
  let open Types in
  { Parsetree.pval_name = mk_var id.Ident.name
  ; pval_type = gen_core_type vd.val_type
  ; pval_prim = []
  ; pval_attributes = vd.val_attributes
  ; pval_loc = Location.none
  }

and gen_type_decl id type_decl =
  let open Types in
  [ Ast_helper.Type.mk
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
         | Type_open -> Parsetree.Ptype_open
         | Type_abstract -> Parsetree.Ptype_abstract
         | Type_record (labels, _) ->
           Parsetree.Ptype_record
             (List.map (fun lbl ->
                  { Parsetree.pld_name = mk_var lbl.ld_id.Ident.name
                  ; pld_mutable = lbl.ld_mutable
                  ; pld_type = gen_core_type lbl.ld_type
                  ; pld_loc = Location.none
                  ; pld_attributes = lbl.ld_attributes
                  })
                 labels)
         | Type_variant cstrs ->
           Parsetree.Ptype_variant
             (List.map (fun c ->
                  { Parsetree.pcd_name = mk_var c.cd_id.Ident.name
                  ; pcd_args = List.map gen_core_type c.cd_args
                  ; pcd_res =
                      (match c.cd_res with
                       | None -> None
                       | Some t -> Some (gen_core_type t))
                  ; pcd_loc = Location.none
                  ; pcd_attributes = c.cd_attributes
                  })
                 cstrs))
      (mk_var id.Ident.name)    ]

and gen_extension_constructor id ext =
  let open Types in
  { Parsetree.pext_name = mk_var id.Ident.name
  ; pext_kind =
      Parsetree.Pext_decl
        (List.map gen_core_type ext.ext_args,
         Option.map gen_core_type ext.ext_ret_type)
  ; pext_loc = Location.none
  ; pext_attributes = ext.ext_attributes
  }

and gen_type_extension id ext =
  let open Types in
  Ast_helper.Te.mk (mk_id (Path.name ext.ext_type_path))
    [ gen_extension_constructor id ext ]

and gen_modtype_decl id modtype_decl =
  let open Types in
  Ast_helper.Mtd.mk
    ?typ:
      (match modtype_decl.mtd_type with
       | None -> None
       | Some t -> Some (gen_modtype t))
    (mk_var id.Ident.name)

and gen_class_descr params path type_ =
  gen_class_infos params path (gen_class_type type_)

and gen_class_decl env params path type_ =
  gen_class_infos params path (gen_class_expr env type_)

and gen_class_infos
  : type a. Types.type_expr list -> Path.t -> a -> a Parsetree.class_infos
  = fun params path expr ->
  let open Types in
  let open Parsetree in
  { Parsetree.pci_virt = Asttypes.Concrete
  ; pci_params = List.map (fun t -> gen_core_type t, Asttypes.Invariant) params
  ; pci_name = mk_var (Path.last path)
  ; pci_expr = expr
  ; pci_loc = Location.none
  ; pci_attributes = []
  }

and gen_class_type cty : Parsetree.class_type =
  let open Types in
  let open Parsetree in
  { pcty_loc = Location.none
  ; pcty_attributes = []
  ; pcty_desc =
      match cty with
      | Cty_constr (path, args, cty) ->
        Parsetree.Pcty_constr
          (mk_var (Untypeast.lident_of_path path),
           List.map gen_core_type args)
      | Cty_signature class_sig ->
        Pcty_signature
          { pcsig_self = gen_core_type class_sig.csig_self
          ; pcsig_fields = [] (* todo *)
          }
      | Cty_arrow (lbl, ty, cty) ->
        Pcty_arrow (lbl, gen_core_type ty, gen_class_type cty)
  }

and gen_class_expr env cty : Parsetree.class_expr =
  let open Types in
  let open Parsetree in
  { pcl_loc = Location.none
  ; pcl_attributes = []
  ; pcl_desc =
      match cty with
      | Cty_constr (path, args, cty) ->
        Parsetree.Pcl_constr
          (mk_var (Untypeast.lident_of_path path),
           List.map gen_core_type args)
      | Cty_arrow (lbl, ty, cty) ->
        let var, env = freevar ty env in
        let var = Ast_helper.Pat.var (mk_var var) in
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
                     (Parsetree.Pcf_method
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
              Types.Vars.fold
                (fun name (is_mutable, is_virtual, ty) lst ->
                   let def =
                     match is_virtual with
                     | Asttypes.Virtual ->
                       Parsetree.Cfk_virtual (gen_core_type ty)
                     | Asttypes.Concrete ->
                       let expr, _ = gen_expr1 env ty in
                       Cfk_concrete (Asttypes.Fresh, expr) in
                   let var =
                     gen_class_field
                       (Parsetree.Pcf_val (mk_var name, is_mutable, def)) in
                   var :: lst)
                class_sig.csig_vars
                [] in
            Pcl_structure
              { pcstr_self = Ast_helper.Pat.any ()
              ; pcstr_fields = vars @ methods
              }
          | _ -> failwith "not an object"
        end
  }

and gen_class_field f =
  { Parsetree.pcf_loc = Location.none
  ; pcf_attributes = []
  ; pcf_desc = f
  }

and gen_sig_item sig_item =
  let open Types in
  match sig_item with
  | Sig_value (id, vd) ->
    Ast_helper.Sig.value (gen_sig_value id vd)
  | Sig_type (id, type_decl, _) ->
    Ast_helper.Sig.type_ (gen_type_decl id type_decl)
  | Sig_typext (id, ext, Text_exception) ->
    Ast_helper.Sig.exception_ (gen_extension_constructor id ext)
  | Sig_typext (id, ext, _) ->
    Ast_helper.Sig.type_extension (gen_type_extension id ext)
  | Sig_modtype (id, modtype_decl) ->
    Ast_helper.Sig.modtype (gen_modtype_decl id modtype_decl)
  | Sig_module (id, mod_decl, _) ->
    Ast_helper.Sig.module_
      (Ast_helper.Md.mk
         (mk_var id.Ident.name)
         (gen_modtype mod_decl.md_type))
  | Sig_class (_, c, _) ->
    Ast_helper.Sig.class_
      [ gen_class_descr c.cty_params c.cty_path c.cty_type ]
  | Sig_class_type (_, c, _) ->
    Ast_helper.Sig.class_type
      [ gen_class_descr c.clty_params c.clty_path c.clty_type ]

and gen_signature_item env sig_item =
  let open Types in
  match sig_item with
  | Sig_value (id, vd) ->
    let expr, _ = gen_expr1 env vd.Types.val_type in
    Ast_helper.Str.value
      Asttypes.Nonrecursive
      [ Ast_helper.Vb.mk
         (Ast_helper.Pat.var (mk_var id.Ident.name))
         expr ]
  | Sig_type (id, type_decl, _) ->
    Ast_helper.Str.type_ (gen_type_decl id type_decl)
  | Sig_typext (id, ext, Text_exception) ->
    Ast_helper.Str.exception_ (gen_extension_constructor id ext)
  | Sig_typext (id, ext, _) ->
    Ast_helper.Str.type_extension (gen_type_extension id ext)
  | Sig_modtype (id, modtype_decl) ->
    Ast_helper.Str.modtype (gen_modtype_decl id modtype_decl)
  | Sig_module (id, mod_decl, _) ->
    Ast_helper.Str.module_
      (Ast_helper.Mb.mk
         (mk_var id.Ident.name)
         (gen_module env mod_decl.md_type))
  | Sig_class (_, c, _) ->
    Ast_helper.Str.class_
      [ gen_class_decl env c.cty_params c.cty_path c.cty_type ]
  | Sig_class_type (_, c, _) ->
    Ast_helper.Str.class_type
      [ gen_class_descr c.clty_params c.clty_path c.clty_type ]


let needs_parentheses e = match e.Parsetree.pexp_desc with
  | Parsetree.Pexp_fun _ -> true
  | _ -> false

let node ~loc ~env parents node =
  match node.t_node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let strs =
      gen_expr ~many:true env ty >>= fun (result, _) ->
      let fmt, to_string = Format.to_string () in
      Pprintast.expression fmt result ;
      let str = to_string () in
      let str = if needs_parentheses result then "(" ^ str ^ ")" else str in
      [ str ] in
    loc, strs
  | Module_expr expr ->
    let ty = expr.Typedtree.mod_type in
    let result = gen_module env ty in
    let fmt, to_string = Format.to_string () in
    Pprintast.default#module_expr fmt result ;
    let str = to_string () in
    loc, [ str ]
  | node ->
    raise (Not_allowed (BrowseT.string_of_node node))
