(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open Parsetree
open Std

let default_loc = function
  | None -> Location.none
  | Some loc -> loc

let mkoptloc opt x =
  match opt with
  | None -> Location.mknoloc x
  | Some l -> Location.mkloc x l

let app a b =
  let loc =
    if a.pexp_loc.Location.loc_ghost
    then {b.pexp_loc with Location.loc_ghost = true}
    else b.pexp_loc
  in
  Ast_helper.Exp.apply ~loc a [Raw_compat.Parsetree.arg_label_of_str "", b]

let pat_app f (pat,expr) = pat, app f expr

let prim_ident prim = Longident.parse ("_." ^ prim)
let prim ?(ghost=true) prim =
  let open Location in
  let ident = mknoloc (prim_ident prim) in
  let ident = if ghost
    then ident
    else {ident with loc = {ident.loc with loc_ghost = false}}
  in
  Ast_helper.Exp.ident ~loc:ident.loc ident

let any_val' = prim "Any.val'"


(* Helpers; extend as needed *)
module Ast = struct
  type type_scheme =
    | Var   of string
    | Arrow of Asttypes.label * type_scheme * type_scheme
    | Tuple of type_scheme list
    | Named of type_scheme list * string
    | Core_type of core_type

  and expr =
    | Fun   of (string * bool) list * expr
    | App   of expr * expr
    | Ident of string
    | AnyVal (* wild card ident *)

  and binding = {
    ident   : string ;
    typesig : type_scheme ;
    body    : expr ;
  }

  and top_item =
    | Binding of binding
    | Module of string * top_item list

  let sub_of_simple_variants lst =
    let variants = List.map lst ~f:(fun s -> Ast_helper.rtag s true []) in
    let ptyp = Ast_helper.Typ.variant variants Asttypes.Open (Some []) in
    Core_type ptyp

  let freshvars  = Stream.from (fun i -> Some (Printf.sprintf "\x00_%d" i))
  let new_var () = Stream.next freshvars
end

open Ast

let unit_ty = Named ([],"unit")
let bool_ty = Named ([],"bool")

let rec translate_ts ?ghost_loc =
  let loc = default_loc ghost_loc in
  function
  | Var "_" -> Ast_helper.Typ.any ~loc ()
  | Var ident -> Ast_helper.Typ.var ~loc ident
  | Arrow (label, a, b) ->
    let a = translate_ts ?ghost_loc a in
    let b = translate_ts ?ghost_loc b in
    Ast_helper.Typ.arrow ~loc (Raw_compat.Parsetree.arg_label_of_str label) a b
  | Tuple lst ->
    let lst = List.map lst ~f:(translate_ts ?ghost_loc) in
    Ast_helper.Typ.tuple ~loc lst
  | Named (params, id) ->
    let id = Longident.parse id in
    let params = List.map (translate_ts ?ghost_loc) params in
    Ast_helper.Typ.constr ~loc (mkoptloc ghost_loc id) params
  | Core_type ct -> ct

and translate_expr ?ghost_loc : Ast.expr -> _ =
  let loc = default_loc ghost_loc in
  function
  | Fun (simple_patterns, body) ->
    List.fold_right simple_patterns ~init:(translate_expr ?ghost_loc body) ~f:(
      fun (simple_pattern, is_label) body ->
        let pat = Ast_helper.Pat.var ~loc (mkoptloc ghost_loc simple_pattern) in
        let lbl = if is_label then simple_pattern else "" in
        Ast_helper.Exp.fun_ ~loc (Raw_compat.Parsetree.arg_label_of_str lbl) None pat body
    )
  | App (f, x) ->
    app (translate_expr ?ghost_loc f) (translate_expr ?ghost_loc x)
  | Ident i ->
    Ast_helper.Exp.ident ~loc (mkoptloc ghost_loc (Longident.parse i))
  | AnyVal -> any_val'


let sig_of_binding ?ghost_loc { ident; typesig; body = _ } =
  let loc = default_loc ghost_loc in
  let pval_ty =
    Ast_helper.Val.mk ~loc (mkoptloc ghost_loc ident)
      (translate_ts ?ghost_loc typesig)
  in
  Ast_helper.Sig.value ~loc pval_ty

let str_of_binding ?ghost_loc { ident; typesig; body } =
  let loc = default_loc ghost_loc in
  let pat = Ast_helper.Pat.var ~loc (mkoptloc ghost_loc ident) in
  let tsig = translate_ts ?ghost_loc typesig in
  let body = translate_expr ?ghost_loc body in
  let pexp = Ast_helper.Exp.constraint_ ~loc body tsig in
  let bind = Ast_helper.Vb.mk ~loc pat pexp in
  Ast_helper.Str.value ~loc Asttypes.Nonrecursive [bind]

let rec str_of_top_lvl ?ghost_loc = function
  | Binding b -> str_of_binding ?ghost_loc b
  | Module (name, m) ->
    let loc = default_loc ghost_loc in
    let me = Ast_helper.Mod.structure ~loc (List.map str_of_top_lvl m) in
    let mb = Ast_helper.Mb.mk (mkoptloc ghost_loc name) me in
    Ast_helper.Str.module_ ~loc mb

let rec sig_of_top_lvl ?ghost_loc = function
  | Binding b -> sig_of_binding ?ghost_loc b
  | Module (name, m) ->
    let loc = default_loc ghost_loc in
    let mty = Ast_helper.Mty.signature ~loc (List.map sig_of_top_lvl m) in
    let md = Ast_helper.Md.mk (mkoptloc ghost_loc name) mty in
    Ast_helper.Sig.module_ ~loc md

(* Lwt extension *)
module Lwt = struct
  let un_lwt = prim "Lwt.un_lwt"
  let to_lwt = prim "Lwt.to_lwt"
  let in_lwt = prim "Lwt.in_lwt"
  let unit_lwt = prim "Lwt.unit_lwt"
  let un_stream = prim "Lwt.un_stream"
  let finally' = prim "Lwt.finally'"
  let raise_lwt' = prim_ident "Lwt.raise_lwt'"
end

(* Js extension *)
module Js = struct
  let un_js     = prim ~ghost:false "Js.un_js"
  let un_meth   = prim ~ghost:false "Js.un_meth"
  let un_constr = prim ~ghost:false "Js.un_constr"
  let un_prop   = prim ~ghost:false "Js.un_prop"
end

(* OUnit extension *)
module OUnit = struct
  let fresh_test_module_ident =
    let counter = ref 0 in
    fun () ->
      incr counter;
      ("_TEST_" ^ string_of_int !counter)

  let force_bool = prim "OUnit.force_bool"
  let force_unit = prim "OUnit.force_unit"
  let force_unit_arrow_unit = prim "OUnit.force_unit_arrow_unit"
  let force_indexed = prim "OUnit.force_indexed"
end

(* tools used in the next few modules *)
let mk_fun ~args =
  Fun (List.map (fun s -> (s, false)) args, App (Ident "Obj.magic", AnyVal))

let mk_labeled_fun ~args = Fun (args, App (Ident "Obj.magic", AnyVal))

type tydecl = string Location.loc * Parsetree.type_declaration

(*module type Type_conv_intf = sig
  val bindings : tydecl -> Ast.binding list
end*)

(** Some extensions (sexp, cow) follow a very simple generation pattern *)
module type Simple_conv_intf = sig
  val t : Ast.type_scheme
  val name : string
end

module Make_simple (Conv : Simple_conv_intf) = struct
  let mk_arrow x y = Arrow ("", x, y)

  let named params ty = Named (params, ty)

  let conv_of_sig params ty =
    let params = Raw_compat.Parsetree.format_params ~f:(fun v -> Var v) params in
    List.fold_right ~f:(fun var acc -> mk_arrow (mk_arrow var Conv.t) acc) params
      ~init:(mk_arrow (named params ty) Conv.t)

  let of_conv_sig params ty =
    let params = Raw_compat.Parsetree.format_params ~f:(fun v -> Var v) params in
    List.fold_right ~f:(fun var acc -> mk_arrow (mk_arrow Conv.t var) acc) params
      ~init:(mk_arrow Conv.t (Named (params, ty)))

  let conv_of_ (located_name, type_infos) =
    let ty = located_name.Location.txt in
    let args =
      let f x = Conv.name ^ "_of_" ^ x in
      Raw_compat.Parsetree.format_params ~f type_infos.ptype_params
    in
    Binding {
      ident = Conv.name ^ "_of_" ^ ty ;
      typesig = conv_of_sig type_infos.ptype_params ty;
      body = mk_fun ~args ;
    }

  let _of_conv (located_name, type_infos) =
    let ty = located_name.Location.txt in
    let args =
      let f x = x ^ "_of_" ^ Conv.name in
      Raw_compat.Parsetree.format_params ~f type_infos.ptype_params
    in
    Binding {
      ident = ty ^ "_of_" ^ Conv.name ;
      typesig = of_conv_sig type_infos.ptype_params ty;
      body = mk_fun ~args ;
    }

  let bindings ty = [ conv_of_ ty ; _of_conv ty ]
end

module Sexp = Make_simple(struct
  let t = Named ([], "Sexplib.Sexp.t")
  let name = "sexp"
end)

module Typerep = struct
  let typename_of_ name ?(suffix=name) params =
    let mk_name x = Named ([x], "Typerep_lib.Typename.t") in
    let typesig =
      List.fold_right ~f:(fun p acc -> Arrow ("", mk_name p, acc)) params
        ~init:(mk_name (Named (params, name)))
    in
    Binding { ident = "typename_of_" ^ suffix ; body = AnyVal ; typesig }

  let mk_rep x = Named ([x], "Typerep_lib.Std.Typerep.t")

  let typerep_of_ name params =
    let typesig =
      List.fold_right ~f:(fun p acc -> Arrow ("", mk_rep p, acc)) params
        ~init:(mk_rep (Named (params, name)))
    in
    Binding { ident = "typerep_of_" ^ name ; body = AnyVal ; typesig }

  let typename_mod name params =
    let named =
      let typesig =
        List.fold_right ~f:(fun p acc -> Arrow ("", mk_rep p, acc)) params
          ~init:(Named ([Named (params, name)], "Typerep_lib.Std.Typerep.Named.t"))
      in
      Binding { ident = "named" ; body = AnyVal ; typesig }
    in
    let bindings = [ typename_of_ name ~suffix:"t" params ; named ] in
    Module ("Typename_of_" ^ name, bindings)

  let top_items ({ Location. txt }, ty_decl) =
    let params =
      Raw_compat.Parsetree.format_params ~f:(fun v -> Var v) ty_decl.ptype_params
    in
    [ typename_mod txt params ; typename_of_ txt params ; typerep_of_ txt params ]
end

(* the Cow generators are parametrized by the extension name *)
let cow_supported_extension ext = List.mem ext ["json"; "xml"; "html";]
module Make_cow (Ext : sig val name : string end) =
  Make_simple(struct
    let t = Named ([], "Cow." ^(String.capitalize Ext.name)^ ".t")
    let name = Ext.name
  end)

module Binprot = struct

  let binding ~prefix ?(suffix="") ~typesig ty =
    let (located_name, ty_infos) = ty in
    let tyname = located_name.Location.txt in
    let args = Raw_compat.Parsetree.format_params ~f:(fun x -> prefix ^ x ^ suffix) ty_infos.ptype_params in
    Binding {
      ident = prefix ^ tyname ^ suffix;
      typesig = typesig ty ;
      body = mk_fun ~args ;
    }

  module Sizer = struct
    let int = Named ([], "int")

    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> x) ty_infos.ptype_params in
      List.fold_right ~f:(fun v acc -> Arrow ("", Arrow ("", Var v, int), acc)) params
        ~init:(Arrow ("", Named (List.map (fun x -> Var x) params, name.Location.txt), int))

    let prefix = "bin_size_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Write = struct
    let writer t = Named ([t], "Bin_prot.Write.writer")

    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> x) ty_infos.ptype_params in
      let t = Named (List.map (fun x -> Var x) params, name.Location.txt) in
      let init = writer t in
      let make_var str = writer (Var str) in
      List.fold_right ~init ~f:(fun v acc -> Arrow ("", make_var v, acc)) params

    let prefix = "bin_write_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Writer = struct
    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> Var x) ty_infos.ptype_params in
      List.fold_right params
        ~init:(Named ([Named (params, name.Location.txt)], "Bin_prot.Type_class.writer"))
        ~f:(fun param acc -> Arrow ("", Named ([param], "Bin_prot.Type_class.writer"), acc))

    let prefix = "bin_writer_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Read = struct
    let reader t = Named ([t], "Bin_prot.Read.reader")
    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> x) ty_infos.ptype_params in
      let init = reader
        (Named (List.map (fun x -> Var x) params, name.Location.txt))
      in
      let make_var str = reader (Var str) in
      List.fold_right ~f:(fun v acc -> Arrow ("", make_var v, acc)) ~init params

    let prefix = "bin_read_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Read__ = struct
    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> x) ty_infos.ptype_params in
      let res = Named (List.map (fun x -> Var x) params, name.Location.txt) in
      let init = Read.reader (Arrow ("", Named ([], "int"), res)) in
      let make_var str = Read.reader (Var str) in
      List.fold_right ~f:(fun v acc -> Arrow ("", make_var v, acc)) ~init params

    let prefix = "__bin_read_"
    let suffix = "__"

    let binding ty = binding ~prefix ~suffix ~typesig ty
  end

  module Reader = struct
    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> Var x) ty_infos.ptype_params in
      List.fold_right params
        ~init:(Named ([Named (params, name.Location.txt)], "Bin_prot.Type_class.reader"))
        ~f:(fun param acc -> Arrow ("", Named ([param], "Bin_prot.Type_class.reader"), acc))

    let prefix = "bin_reader_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Type_class = struct
    let typesig (name, ty_infos) =
      let params = Raw_compat.Parsetree.format_params ~f:(fun x -> Var x) ty_infos.ptype_params in
      List.fold_right params
        ~init:(Named ([Named (params, name.Location.txt)], "Bin_prot.Type_class.t"))
        ~f:(fun param acc -> Arrow ("", Named ([param], "Bin_prot.Type_class.t"), acc))

    let prefix = "bin_"

    let binding ty = binding ~prefix ~typesig ty
  end
end

module FV_helpers = struct
  let linear_pass ?result ~name ~body lst ret_ty =
    let init =
      match result with
      | None -> ret_ty
      | Some ty -> ty
    in
    let typesig =
      List.fold_right lst ~init ~f:(fun cstr acc_ty ->
        Arrow (cstr.ident, Arrow ("", cstr.typesig, ret_ty), acc_ty)
      )
    in
    Binding { ident = name ; typesig ; body }

  let mk_fold ~body elts =
    let typesig =
      let a = new_var () in
      let init_ty, arrows =
        List.fold_right elts ~init:(a, Var a) ~f:(
          fun cstr (fun_res, acc) ->
            let param = new_var () in
            let f =
              Arrow ("", Var param, Arrow ("", cstr.typesig, Var fun_res))
            in
            (param, Arrow (cstr.ident, f, acc))
        )
      in
      Arrow ("init", Var init_ty, arrows)
    in
    let body = Fun (["init", true], body) in
    Binding { ident = "fold" ; typesig ; body }
end

(* TODO: factorize [Variants] and [Fields] *)
module Variants = struct
  let mk_cstr_typesig ~self args res_opt =
    let r = Option.value_map res_opt ~default:self ~f:(fun x -> Core_type x) in
    List.fold_right args ~init:r ~f:(fun arg ret_ty ->
      Arrow ("", Core_type arg, ret_ty)
    )

  let constructors ~self cstrs =
    Raw_compat.Parsetree.map_constructors cstrs ~f:(
      fun name args res_opt loc ->
        let typesig = mk_cstr_typesig ~self args res_opt in
        Binding { ident = String.lowercase name ; typesig ; body = AnyVal }
    )

  let mk_module ~self cstrs =
    let cstrs_dot_t =
      Raw_compat.Parsetree.map_constructors cstrs ~f:(
        fun name args res_opt loc ->
          let t = Named ([mk_cstr_typesig ~self args res_opt], "Variantslib.Variant.t") in
          { ident = String.lowercase name ; typesig = t ; body = AnyVal }
      )
    in

    let body =
      mk_labeled_fun
        (Raw_compat.Parsetree.map_constructors cstrs
          ~f:(fun name _ _ _ -> String.lowercase name, true))
    in

    let fold = FV_helpers.mk_fold ~body cstrs_dot_t in

    let linear_pass ?result ~name ty =
      FV_helpers.linear_pass ?result ~name cstrs_dot_t ~body ty
    in

    let iter = linear_pass ~name:"iter" unit_ty in

    let map =
      let typesig =
        let ret_ty = Var (new_var ()) in
        List.fold_right2 cstrs_dot_t cstrs ~init:ret_ty ~f:(
          fun cstr c acc ->
            let args = Raw_compat.Parsetree.args_of_constructor c in
            let tmp =
              List.fold_right args ~init:ret_ty
                ~f:(fun arg acc -> Arrow ("", Core_type arg, acc))
            in
            Arrow (cstr.ident, Arrow ("", cstr.typesig, tmp), acc)
        )
      in
      let typesig = Arrow ("", self, typesig) in
      let body =
        let args = List.map cstrs_dot_t ~f:(fun b -> b.ident, true) in
        Fun ((new_var (), false) :: args, AnyVal)
      in
      Binding { ident = "map" ; typesig ; body }
    in

    let descriptions =
      let (!) x = Named ([], x) in
      Binding {
        ident = "descriptions" ;
        typesig = Named ([Tuple [ !"string" ; !"int" ]], "list") ;
        body = AnyVal ;
      }
    in

    Module ("Variants", List.map cstrs_dot_t ~f:(fun b -> Binding b) @ [
      fold ; iter ; map ; descriptions
    ])

  let top_lvl ({ Location.txt = name },ty) =
    let params = Raw_compat.Parsetree.format_params ~f:(fun s -> Var s) ty.ptype_params in
    let self = Named (params, name) in
    match ty.ptype_kind with
    | Parsetree.Ptype_variant cstrs ->
      constructors ~self cstrs @ [mk_module ~self cstrs]
    | _ -> []
end

module Fields = struct
  let gen_field self lbl : top_item list =
    let ({ Location.txt = name }, mut, ty, _) =
      Raw_compat.Parsetree.inspect_label lbl
    in
    (* Remove higher-rank quantifiers *)
    let ty = match ty.ptyp_desc with Ptyp_poly (_,ty) -> ty | _ -> ty in
    let ty = Core_type ty in
    let accessor = Arrow ("", self, ty) in
    let fields = [Binding { ident = name; typesig = accessor; body = AnyVal }] in
    match mut with
    | Asttypes.Immutable -> fields
    | Asttypes.Mutable ->
      let typesig = Arrow ("", self, Arrow ("", ty, unit_ty)) in
      (Binding { ident = "set_" ^ name; typesig ; body = AnyVal }) :: fields

  let make_fields_module ~name ~self fields : top_item =
    let names =
      let typesig = Named ([Named ([], "string")], "list") in
      Binding { ident = "names" ; typesig ; body = AnyVal }
    in

    let fields_dot_t =
      let perms = sub_of_simple_variants [ "Read" ; "Set_and_create" ] in
      List.map fields ~f:(fun lbl ->
        let ({ Location.txt = name }, _, ty, _) =
          Raw_compat.Parsetree.inspect_label lbl
        in
        let ty = match ty.ptyp_desc with Ptyp_poly (_,ty) -> ty | _ -> ty in
        let t = Named ([ perms ; self ; Core_type ty ], "Fieldslib.Field.t_with_perm") in
        { ident = name ; typesig = t ; body = AnyVal }
      )
    in

    (* Helper, used in the next few functions *)
    let body =
      mk_labeled_fun (List.map fields ~f:(fun lbl ->
        let (l,_,_,_) = Raw_compat.Parsetree.inspect_label lbl in
        l.Location.txt,true)
      )
    in

(*
    That is so ugly.

    val make_creator :
      x:(([< `Read | `Set_and_create ], t, int) Field.t_with_perm -> 'a -> ('b -> int) * 'c) ->
      y:(([< `Read | `Set_and_create ], t, float ref) Field.t_with_perm -> 'c -> ('b -> float ref) * 'd) ->
      'a -> ('b -> t) * 'd
*)
    let make_creator =
      let acc_ret_ty = Var (new_var ()) in
      let ios, first_input =
        List.fold_right fields ~init:([], acc_ret_ty) ~f:(fun _f (lst, acc) ->
          let x = Var (new_var ()) in
          (x, acc) :: lst, x
        )
      in
      let creator_input = Var (new_var ()) in
      let init =
        let creator = Arrow ("", creator_input, self) in
        Arrow ("", first_input, Tuple [ creator ; acc_ret_ty ])
      in
      let lst =
        List.map2 fields fields_dot_t ~f:(fun f fdt ->
          let (name,_,ty,_) = Raw_compat.Parsetree.inspect_label f in
          (name.Location.txt, ty, fdt)
        )
      in
      let typesig =
        List.fold_right2 lst ios ~init ~f:(fun (name, ty, f_dot_t) (i, o) acc ->
          let field_creator = Arrow ("", creator_input, Core_type ty) in
          Arrow (
            name,
            Arrow ("", f_dot_t.typesig, Arrow ("", i, Tuple [ field_creator ; o ])),
            acc
          )
        )
      in
      Binding { ident = "make_creator" ; typesig ; body }
    in

    let create =
      let typesig =
        List.fold_right fields ~init:self ~f:(fun f acc ->
          let (name, _, t, _) = Raw_compat.Parsetree.inspect_label f in
          Arrow (name.Location.txt, Core_type t, acc)
        )
      in
      Binding { ident = "create" ; typesig ; body }
    in

    let linear_pass ?result ~name ret_ty =
      FV_helpers.linear_pass ?result ~name fields_dot_t ~body ret_ty
    in

    let iter = linear_pass ~name:"iter" unit_ty in
    let forall = linear_pass ~name:"for_all" bool_ty in
    let exists = linear_pass ~name:"exists" bool_ty in
    let to_list =
      let ty_var = Var (new_var ()) in
      linear_pass ~result:(Named ([ty_var], "list")) ~name:"to_list" ty_var
    in

    let fold = FV_helpers.mk_fold ~body fields_dot_t in

    let map =
      let typesig =
        List.fold_right2 fields_dot_t fields ~init:self ~f:(
          fun field_t f acc_ty ->
            let (_, _, ty, _) = Raw_compat.Parsetree.inspect_label f in
            let ty = match ty.ptyp_desc with Ptyp_poly (_,ty) -> ty | _ -> ty in
            Arrow (field_t.ident,
              Arrow ("", field_t.typesig, Core_type ty), acc_ty
            )
        )
      in
      Binding { ident = "map" ; typesig ; body }
    in

    let map_poly =
      let var = Var (new_var ()) in
      let perms = sub_of_simple_variants [ "Read" ; "Set_and_create" ] in
      let user = Named ([ perms ; self ; var ], "Fieldslib.Field.user") in
      let typesig = Arrow ("", user, Named ([var], "list")) in
      Binding { ident = "map_poly" ; typesig ; body = AnyVal }
    in

    let direct_iter =
      let typesig =
        List.fold_right2 fields_dot_t fields ~init:unit_ty ~f:(
          fun cstr f acc_ty ->
            let (_, _, ty, _) = Raw_compat.Parsetree.inspect_label f in
            let ty = match ty.ptyp_desc with Ptyp_poly (_,ty) -> ty | _ -> ty in
            let f =
              let tail = Arrow ("", self, Arrow ("", Core_type ty, unit_ty)) in
              Arrow ("", cstr.typesig, tail)
            in
            Arrow (cstr.ident, f, acc_ty)
        )
      in
      let body = Fun (["t", false], body) in
      Binding { ident = "iter" ; typesig = Arrow ("", self, typesig); body }
    in

    let direct_fold =
      let typesig =
        let a = new_var () in
        let init_ty, arrows =
          List.fold_right2 fields_dot_t fields ~init:(a, Var a) ~f:(
            fun cstr f (fun_res, acc) ->
              let (_, _, ty, _) = Raw_compat.Parsetree.inspect_label f in
              let ty = match ty.ptyp_desc with Ptyp_poly (_,ty) -> ty | _ -> ty in
              let param = new_var () in
              let f =
                let tail = Arrow ("", self, Arrow ("", Core_type ty, Var fun_res)) in
                Arrow ("", Var param, Arrow ("", cstr.typesig, tail))
              in
              (param, Arrow (cstr.ident, f, acc))
          )
        in
        Arrow ("", self, Arrow ("init", Var init_ty, arrows))
      in
      let body = Fun (["t", false; "init", true], body) in
      Binding { ident = "fold" ; typesig ; body }
    in

    Module (
      (if name = "t" then "Fields" else "Fields_of_" ^ name),
      names :: List.map fields_dot_t ~f:(fun x -> Binding x) @ [
        make_creator ; create ; iter ; map ; fold ; map_poly ; forall ; exists ;
        to_list ; Module ("Direct", [ direct_iter ; direct_fold ])
      ]
    )

  let top_lvl ({ Location.txt = name },ty) =
    let params =
      Raw_compat.Parsetree.format_params ~f:(fun v -> Var v)
        ty.ptype_params
    in
    let self = Named (params, name) in
    match ty.ptype_kind with
    | Parsetree.Ptype_record lst ->
      List.concat_map ~f:(gen_field self) lst @
      [make_fields_module ~name ~self lst]
    | _ -> []

end

module Enumerate = struct
  let top_lvl ({ Location.txt = name },ty) =
    let ident = if name = "t" then "all" else "all_of_" ^ name in
    let typesig =
      let params =
        Raw_compat.Parsetree.format_params ~f:(fun v -> Var v)
          ty.ptype_params
      in
      let param =
        Named (params, name)
      in
      List.fold_right params ~init:(Named ([param], "list")) ~f:(fun var acc ->
        Arrow ("", var, acc)
      )
    in
    Binding { ident ; typesig ; body = AnyVal }
end

module Compare = struct
  let mk_simpl t = Arrow ("", t, Arrow ("", t, Named ([], "int")))

  let bindings ~kind ({ Location.txt = name },ty) =
    let params =
      Raw_compat.Parsetree.format_params ~f:(fun v -> Var v)
        ty.ptype_params
    in
    let self = Named (params, name) in
    let cmp = {
      ident = "compare_" ^ name;
      typesig =
        List.fold_right params ~init:(mk_simpl self) ~f:(fun param t ->
          Arrow ("", mk_simpl param, t)
        );
      body = AnyVal
    } in
    match kind with
    | `Def when name = "t" -> [ Binding {cmp with ident = "compare"}; Binding cmp]
    | `Sig when name = "t" -> [Binding {cmp with ident = "compare"}]
    | _ -> [Binding cmp]

end

module TypeWith = struct
  type generator = string

  let generate_bindings ~kind ~ty = function
    | "sexp" -> List.concat_map ~f:Sexp.bindings ty
    | "sexp_of" -> List.map ~f:Sexp.conv_of_ ty
    | "of_sexp" -> List.map ~f:Sexp._of_conv ty

    | "bin_write" ->
      let open Binprot in
      List.concat_map ~f:(fun ty ->
          [ Sizer.binding ty ;
            Write.binding ty ;
            Writer.binding ty ]
      ) ty

    | "bin_read" ->
      let open Binprot in
      List.concat_map ~f:(fun ty ->
        [ Read.binding ty ;
          Read__.binding ty ;
          Reader.binding ty ]
      ) ty

    | "bin_io" ->
      let open Binprot in
      List.concat_map ~f:(fun ty ->
        [
          Sizer.binding ty ;
          Write.binding ty ;
          (*Write_.binding ty ;*)
          Writer.binding ty ;
          Read.binding ty ;
          (*Read_.binding ty ;*)
          Read__.binding ty ;
          Reader.binding ty ;
          Type_class.binding ty ;
        ]
      ) ty

    | "typerep" ->
      List.concat_map ~f:Typerep.top_items ty

    | "fields" ->
      List.concat_map ~f:Fields.top_lvl ty

    | "variants" ->
      List.concat_map ~f:Variants.top_lvl ty

    | "compare" ->
      List.concat_map ~f:(Compare.bindings ~kind) ty

    | "enumerate" ->
      List.map ~f:Enumerate.top_lvl ty

    | ext when cow_supported_extension ext ->
      let module Cow = Make_cow(struct let name = ext end) in
      List.concat_map ~f:Cow.bindings ty

    | _unsupported_ext -> []

  let generate_definitions ~ty ?ghost_loc ext =
    let bindings = List.concat_map ~f:(generate_bindings ~kind:`Def ~ty) ext in
    List.map (str_of_top_lvl ?ghost_loc) bindings

  let generate_sigs ~ty ?ghost_loc ext =
    let bindings = List.concat_map ~f:(generate_bindings ~kind:`Sig ~ty) ext in
    List.map (sig_of_top_lvl ?ghost_loc) bindings
end

module Nonrec = struct
  let type_nonrec_prefix = "\x00nonrec" (*"__nonrec_"*)
  let type_nonrec_prefix_l = String.length type_nonrec_prefix
  let add id =
    { id with Location.txt = type_nonrec_prefix ^ id.Location.txt }

  let is t =
    let l = String.length t in
    (l > type_nonrec_prefix_l) &&
    try
      for i = 0 to type_nonrec_prefix_l - 1 do
        if t.[i] <> type_nonrec_prefix.[i] then raise Not_found;
      done;
      true
    with Not_found -> false

  let drop t =
    if is t
    then
      let l = String.length t in
      String.sub t type_nonrec_prefix_l (l - type_nonrec_prefix_l)
    else t

  let drop_loc t =
    if is t.Location.txt then
      {t with Location.txt = drop t.Location.txt}
    else
      t
end

(* Custom printf extension *)
module Custom_printf = struct

  let bang loc_start loc_end expr = match expr with
    | { pexp_desc = Pexp_constant (Asttypes.Const_string _ as cs) } ->
      let _str = Raw_compat.extract_const_string cs in
      let loc = {any_val'.pexp_loc with Location. loc_start; loc_end} in
      Some (Ast_helper.Exp.constraint_ ~loc {any_val' with pexp_loc = loc}
              (Ast_helper.Typ.any ~loc ()))
    | _ -> None
end


(* MetaOCaml support *)
module Meta = struct
  let prim_code = prim "Meta.code"
  let prim_uncode = prim "Meta.uncode"

  let code loc_start loc_end expr =
    let loc = {expr.pexp_loc with Location. loc_start; loc_end} in
    Ast_helper.Exp.apply ~loc prim_code ["", expr]

  let uncode loc_start loc_end expr =
    let loc = {expr.pexp_loc with Location. loc_start; loc_end} in
    Ast_helper.Exp.apply ~loc prim_uncode ["", expr]
end
