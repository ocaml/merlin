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

open Parsetree

let default_loc = function
  | None -> Location.none
  | Some loc -> loc

let mkoptloc opt x =
  match opt with
  | None -> Location.mknoloc x
  | Some l -> Location.mkloc x l

let app a b =
  let pexp_loc = { b.pexp_loc with Location.loc_ghost = true } in
  { pexp_desc = Pexp_apply (a, ["", b]) ; pexp_loc }

let pat_app f (pat,expr) = pat, app f expr

let prim_ident prim = Longident.parse ("_." ^ prim)
let prim prim = {
  pexp_desc = Pexp_ident (Location.mknoloc (prim_ident prim));
  pexp_loc = Location.none
}

let any_val' = prim "Any.val'"


(* Helpers; extend as needed *)
module Ast = struct
  type type_scheme = [
    | `Var   of string
    | `Arrow of Asttypes.label * type_scheme * type_scheme
    | `Named of type_scheme list * string
  ]

  and expr = [
    | `Fun   of string list * expr
    | `App   of expr * expr
    | `Ident of string
    | `AnyVal (* wild card ident *)
  ]

  and binding = {
    ident   : string ;
    typesig : type_scheme ;
    body    : expr ;
  }
end
open Ast

let rec translate_ts ?ghost_loc = function
  | `Var ident ->
    { ptyp_desc = Ptyp_var ident ; ptyp_loc = default_loc ghost_loc }
  | `Arrow (label, a, b) ->
    let a = translate_ts ?ghost_loc a in
    let b = translate_ts ?ghost_loc b in
    { ptyp_desc = Ptyp_arrow(label, a, b) ; ptyp_loc = default_loc ghost_loc }
  | `Named (params, id) ->
    let id = Longident.parse id in
    let params = List.map (translate_ts ?ghost_loc) params in
    {
      ptyp_desc = Ptyp_constr (mkoptloc ghost_loc id, params) ;
      ptyp_loc = default_loc ghost_loc ;
    }

and translate_expr ?ghost_loc : Ast.expr -> _ = function
  | `Fun (simple_patterns, body) ->
    List.fold_right
      (fun simple_pattern body ->
        let patt = {
          ppat_desc = Ppat_var (mkoptloc ghost_loc simple_pattern) ;
          ppat_loc = default_loc ghost_loc ;
        }
        in
        {
          pexp_desc = Pexp_function ("", None, [patt, body]) ;
          pexp_loc = default_loc ghost_loc ;
        })
      simple_patterns
      (translate_expr ?ghost_loc body)
  | `App (f, x) ->
    app (translate_expr ?ghost_loc f) (translate_expr ?ghost_loc x)
  | `Ident i -> {
      pexp_desc = Pexp_ident (mkoptloc ghost_loc (Longident.parse i)) ;
      pexp_loc = default_loc ghost_loc ;
    }
  | `AnyVal -> any_val'


let sig_of_binding ?ghost_loc { ident; typesig; body = _ } =
  let pval_ty = {
    pval_type = translate_ts ?ghost_loc typesig ;
    pval_prim = [] ;
    pval_loc  = default_loc ghost_loc ;
  } in
  let psig_desc = Psig_value (mkoptloc ghost_loc ident, pval_ty) in
  { psig_desc ; psig_loc = default_loc ghost_loc }

let str_of_binding ?ghost_loc { ident; typesig; body } =
  let pat = {
    ppat_desc = Ppat_var (mkoptloc ghost_loc ident) ;
    ppat_loc = default_loc ghost_loc ;
  }
  in
  let typesig_opt = Some (translate_ts ?ghost_loc typesig) in
  let body = translate_expr ?ghost_loc body in
  let pexp = {
    pexp_desc = Pexp_constraint (body, typesig_opt, None) ;
    pexp_loc = default_loc ghost_loc ;
  }
  in
  {
    pstr_desc = Pstr_value (Asttypes.Nonrecursive, [(pat, pexp)]) ;
    pstr_loc = default_loc ghost_loc ;
  }

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
  let un_js     = prim "Js.un_js"
  let un_meth   = prim "Js.un_meth"
  let un_constr = prim "Js.un_constr"
  let un_prop   = prim "Js.un_prop"
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
end

(* tools used in the next few modules *)
let format_params ~f =
  List.map (function None -> f "_" | Some id -> f id.Location.txt)

let mk_fun ~args = `Fun (args, `App (`Ident "Obj.magic", `AnyVal))

type tydecl = string Location.loc * Parsetree.type_declaration

module type Type_conv_intf = sig
  val bindings : tydecl -> Ast.binding list
end

(** Some extensions (sexp, cow) follow a very simple generation pattern *)
module type Simple_conv_intf = sig
  val t : [> `Named of 'a list * string]
  val name : string
end

module Make_simple (Conv : Simple_conv_intf) = struct
  let mk_arrow x y = `Arrow ("", x, y)
    
  let named params ty = `Named (params, ty)
    
  let conv_of_sig params ty =
    let params = format_params ~f:(fun v -> `Var v) params in
    List.fold_right (fun var acc -> mk_arrow (mk_arrow var Conv.t) acc) params
      (mk_arrow (named params ty) Conv.t)
      
  let of_conv_sig params ty =
    let params = format_params ~f:(fun v -> `Var v) params in
    List.fold_right (fun var acc -> mk_arrow (mk_arrow Conv.t var) acc) params
      (mk_arrow Conv.t (`Named (params, ty)))

  let conv_of_ (located_name, type_infos) =
    let ty = located_name.Location.txt in
    let args =
      let f x = Conv.name ^ "_of_" ^ x in
      format_params ~f type_infos.ptype_params
    in
    {
      ident = Conv.name ^ "_of_" ^ ty ;
      typesig = conv_of_sig type_infos.ptype_params ty;
      body = mk_fun ~args ;
    }
      
  let _of_conv (located_name, type_infos) =
    let ty = located_name.Location.txt in
    let args =
      let f x = x ^ "_of_" ^ Conv.name in
      format_params ~f type_infos.ptype_params
    in
    {
      ident = ty ^ "_of_" ^ Conv.name ;
      typesig = of_conv_sig type_infos.ptype_params ty;
      body = mk_fun ~args ;
    }

  let bindings ty = [ conv_of_ ty ; _of_conv ty ]
end

module Sexp = Make_simple(struct
  let t = `Named ([], "Sexplib.Sexp.t")
  let name = "sexp"
end)

(* the Cow generators are parametrized by the extension name *)
let cow_supported_extension ext = List.mem ext ["json"; "xml"; "html";] 
module Make_cow (Ext : sig val name : string end) =
  Make_simple(struct
    let t = `Named ([], "Cow." ^(String.capitalize Ext.name)^ ".t")
    let name = Ext.name
  end)

module Binprot = struct

  let binding ~prefix ~typesig ty =
    let (located_name, ty_infos) = ty in
    let tyname = located_name.Location.txt in
    let args = format_params ~f:(fun x -> prefix ^ x) ty_infos.ptype_params in
    {
      ident = prefix ^ tyname ;
      typesig = typesig ty ;
      body = mk_fun ~args ;
    }

  module Sizer = struct
    let int = `Named ([], "int")

    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> x) ty_infos.ptype_params in
      List.fold_right (fun v acc -> `Arrow ("", `Arrow ("", `Var v, int), acc)) params
        (`Arrow ("", `Named (List.map (fun x -> `Var x) params, name.Location.txt), int))

    let prefix = "bin_size_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Write = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> x) ty_infos.ptype_params in
      let acc =
        `Arrow ("", `Named ([], "Bin_prot.Common.buf"),
        `Arrow ("pos", `Named ([], "Bin_prot.Common.pos"),
        `Arrow ("", `Named (List.map (fun x -> `Var x) params, name.Location.txt),
        `Named ([], "Bin_prot.Common.pos"))))
      in
      let make_var str =
        `Arrow ("", `Named ([], "Bin_prot.Unsafe_common.sptr"),
        `Arrow ("", `Named ([], "Bin_prot.Unsafe_common.eptr"),
        `Arrow ("", `Var str,
        `Named ([], "Bin_prot.Unsafe_common.sptr"))))
      in
      List.fold_right (fun v acc -> `Arrow ("", make_var v, acc)) params acc

    let prefix = "bin_write_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Writer = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.writer"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.writer"))

    let prefix = "bin_writer_"
  
    let binding ty = binding ~prefix ~typesig ty
  end

  module Read = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> x) ty_infos.ptype_params in
      let acc =
        `Arrow ("", `Named ([], "Bin_prot.Common.buf"),
        `Arrow ("pos_ref", `Named ([`Named ([], "Bin_prot.Common.pos")], "ref"),
        `Named (List.map (fun x -> `Var x) params, name.Location.txt)))
      in
      let make_var str =
        `Arrow ("", `Named ([], "Bin_prot.Unsafe_common.sptr_ptr"),
        `Arrow ("", `Named ([], "Bin_prot.Unsafe_common.eptr"),
        `Var str))
      in
      List.fold_right (fun v acc -> `Arrow ("", make_var v, acc)) params acc

    let prefix = "bin_read_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Reader = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.reader"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.reader"))

    let prefix = "bin_reader_"

    let binding ty = binding ~prefix ~typesig ty
  end

  module Type_class = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.t"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.t"))

    let prefix = "bin_"

    let binding ty = binding ~prefix ~typesig ty
  end
end

module TypeWith = struct
  type generator = string

  let generate_bindings ~ty = function
    | "sexp" -> Misc.list_concat_map Sexp.bindings ty
    | "sexp_of" -> List.map (fun ty -> Sexp.conv_of_ ty) ty
    | "of_sexp" -> List.map (fun ty -> Sexp._of_conv ty) ty

    | "bin_write" ->
      let open Binprot in
      Misc.list_concat_map (fun ty ->
        [ Sizer.binding ty ; Write.binding ty ; Writer.binding ty ]
      ) ty

    | "bin_read" ->
      let open Binprot in
      Misc.list_concat_map (fun ty ->
        [ Read.binding ty ; Reader.binding ty ]
      ) ty

    | "bin_io" ->
      let open Binprot in
      Misc.list_concat_map (fun ty ->
        [
          Sizer.binding ty ;
          Write.binding ty ;
          Writer.binding ty ;
          Read.binding ty ;
          Reader.binding ty ;
          Type_class.binding ty ;
        ]
      ) ty

    | ext when cow_supported_extension ext ->
      let module Cow = Make_cow(struct let name = ext end) in
      Misc.list_concat_map (fun ty -> Cow.bindings ty) ty

    | _unsupported_ext -> []

  let generate_definitions ~ty ?ghost_loc ext =
    let bindings = Misc.list_concat_map (generate_bindings ~ty) ext in
    List.map (str_of_binding ?ghost_loc) bindings

  let generate_sigs ~ty ?ghost_loc ext =
    let bindings = Misc.list_concat_map (generate_bindings ~ty) ext in
    List.map (sig_of_binding ?ghost_loc) bindings
end

let type_nonrec_prefix = "\x00nonrec" (*"__nonrec_"*)
let type_nonrec_prefix_l = String.length type_nonrec_prefix
let type_add_nonrec id = 
  { id with Location.txt = type_nonrec_prefix ^ id.Location.txt }

let type_is_nonrec t =
  let l = String.length t in
  (l > type_nonrec_prefix_l) &&
  try
    for i = 0 to type_nonrec_prefix_l - 1 do
      if t.[i] <> type_nonrec_prefix.[i] then raise Not_found;
    done;
    true
  with Not_found -> false

let type_drop_nonrec t = 
  let l = String.length t in
  if type_is_nonrec t
  then String.sub t type_nonrec_prefix_l (l - type_nonrec_prefix_l)
  else t

let type_ident_drop_nonrec id =
  if type_is_nonrec id.Ident.name
  then { id with Ident.name = type_drop_nonrec id.Ident.name }
  else id
