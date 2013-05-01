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

  type str_item = [
    | `Let of expr binding
  ]
  and sig_item = [
    | `Val of unit binding
  ]
  and expr = [
    | `Fun   of string list * expr
    | `App   of expr * expr
    | `Ident of string
    | `AnyVal (* wild card ident *)
  ]
  and 'a binding = {
    ident   : string ;
    typesig : type_scheme ;
    body    : 'a ;
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

let translate_declaration ?ghost_loc : Ast.sig_item -> _  = function
  | `Val { ident; typesig; _ } ->
    let pval_ty = {
      pval_type = translate_ts ?ghost_loc typesig ;
      pval_prim = [] ;
      pval_loc  = default_loc ghost_loc ;
    } in
    let psig_desc = Psig_value (mkoptloc ghost_loc ident, pval_ty) in
    { psig_desc ; psig_loc = default_loc ghost_loc }

let rec translate_binding ?ghost_loc { ident ; typesig ; body } =
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
  (pat, pexp)

and translate_implementation ?ghost_loc : Ast.str_item -> _ = function
  | `Let lst ->
    let p = Pstr_value (Asttypes.Nonrecursive, [translate_binding lst]) in
    { pstr_desc = p ; pstr_loc = default_loc ghost_loc }

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


module Lwt = struct
  let un_lwt = prim "Lwt.un_lwt"
  let to_lwt = prim "Lwt.to_lwt"
  let in_lwt = prim "Lwt.in_lwt"
  let unit_lwt = prim "Lwt.unit_lwt"
  let un_stream = prim "Lwt.un_stream"
  let finally' = prim "Lwt.finally'"
  let raise_lwt' = prim_ident "Lwt.raise_lwt'"
end

(* tools used in the next few modules *)
let format_params ~f =
  List.map (function None -> f "_" | Some id -> f id.Location.txt)

let mk_fun ~args = `Fun (args, `App (`Ident "Obj.magic", `AnyVal))


module type Simple_conv_intf = sig
  type ty = string Location.loc * Parsetree.type_declaration
  module Struct : sig
    val make_funs : ty -> Ast.str_item list
  end
  module Sig : sig
    val make_decls : ty -> Ast.sig_item list
  end
end

(** Sig to encode primitive type conv extensions like sexp or cow *)
module type Simple_conv = sig
  val t : [> `Named of 'a list * string]
  val _name_ : string
end

module Make_conv (Conv : Simple_conv) = struct
  type ty = string Location.loc * Parsetree.type_declaration

  include Conv

  module TypeSig = struct
    let mk_arrow x y = `Arrow ("", x, y)

    let named params ty = `Named (params, ty)

    let conv_of params ty =
      let params = format_params ~f:(fun v -> `Var v) params in
      List.fold_right (fun var acc -> mk_arrow (mk_arrow var t) acc) params
        (mk_arrow (named params ty) t)

    let of_conv params ty =
      let params = format_params ~f:(fun v -> `Var v) params in
      List.fold_right (fun var acc -> mk_arrow (mk_arrow t var) acc) params
        (mk_arrow t (`Named (params, ty)))
  end

  module Struct = struct
    let conv_of_ (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let args = format_params ~f:(fun x -> _name_ ^ "_of_" ^ x) type_infos.ptype_params
      in
      `Let {
        ident = _name_ ^ "_of_" ^ ty ;
        typesig = TypeSig.conv_of type_infos.ptype_params ty;
        body = mk_fun ~args ;
      }

    let _of_conv (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let args = format_params ~f:(fun x -> x ^ "_of_" ^ _name_) type_infos.ptype_params
      in
      `Let {
        ident = ty ^ "_of_" ^ _name_ ;
        typesig = TypeSig.of_conv type_infos.ptype_params ty;
        body = mk_fun ~args ;
      }

    let make_funs ty = [ conv_of_ ty ; _of_conv ty ]
  end

  module Sig = struct
    let conv_of_ (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let typesig = TypeSig.conv_of type_infos.ptype_params ty in
      `Val {
        ident = _name_ ^ "_of_" ^ ty ;
        typesig ;
        body = () ;
      }

    let _of_conv (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let typesig = TypeSig.of_conv type_infos.ptype_params ty in
      `Val {
        ident = ty ^ "_of_" ^ _name_ ;
        typesig ;
        body = () ;
      }

    let make_decls ty = [ conv_of_ ty ; _of_conv ty ]
  end
end

module Sexp_conv : Simple_conv = struct
  let t = `Named ([], "Sexplib.Sexp.t")
  let _name_ = "sexp"
end

(* only sealing Sexp for to copy make new Sexp identical to old Verbatim *)
module Sexp = (Make_conv(Sexp_conv) : Simple_conv_intf)

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

  let struct_of binding = `Let binding
  let sig_of binding = `Val { binding with body = () }

  module Sizer = struct
    let int = `Named ([], "int")

    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> x) ty_infos.ptype_params in
      List.fold_right (fun v acc -> `Arrow ("", `Arrow ("", `Var v, int), acc)) params
        (`Arrow ("", `Named (List.map (fun x -> `Var x) params, name.Location.txt), int))

    let prefix = "bin_size_"

    let make_struct ty = struct_of (binding ~prefix ~typesig ty)
    let make_sig ty = sig_of (binding ~prefix ~typesig ty)
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

    let make_struct ty = struct_of (binding ~prefix ~typesig ty)
    let make_sig ty = sig_of (binding ~prefix ~typesig ty)
  end

  module Writer = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.writer"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.writer"))

    let prefix = "bin_writer_"
  
    let make_struct ty = struct_of (binding ~prefix ~typesig ty)
    let make_sig ty = sig_of (binding ~prefix ~typesig ty)
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

    let make_struct ty = struct_of (binding ~prefix ~typesig ty)
    let make_sig ty = sig_of (binding ~prefix ~typesig ty)
  end

  module Reader = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.reader"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.reader"))

    let prefix = "bin_reader_"

    let make_struct ty = struct_of (binding ~prefix ~typesig ty)
    let make_sig ty = sig_of (binding ~prefix ~typesig ty)
  end
end

module Cow = struct
  let supported_extension ext = List.mem ext ["json"; "xml"; "html";]
  let make_cow ~ext = 
    let module M = Make_conv(struct
        let t = `Named ([], "Cow." ^(String.capitalize ext)^ ".t")
        let _name_ = ext
    end) in (module M : Simple_conv_intf)
end

module TypeWith = struct
  type generator = string

  let rec generate_definitions ~ty ?ghost_loc = function
    | "sexp" ->
      let funs = Misc.list_concat_map Sexp.Struct.make_funs ty in
      List.map (translate_implementation ?ghost_loc) funs

    | "bin_write" ->
      let funs =
        let open Binprot in
        Misc.list_concat_map (fun ty ->
          [ Sizer.make_struct ty ; Write.make_struct ty ; Writer.make_struct ty ]
        ) ty
      in
      List.map (translate_implementation ?ghost_loc) funs

    | "bin_read" ->
      let funs =
        let open Binprot in
        Misc.list_concat_map (fun ty -> [ Read.make_struct ty ; Reader.make_struct ty ]) ty
      in
      List.map (translate_implementation ?ghost_loc) funs


    | "bin_io" ->
      let funs =
        let open Binprot in
        Misc.list_concat_map (fun ty ->
          [
            Sizer.make_struct ty ;
            Write.make_struct ty ;
            Writer.make_struct ty ;
            Read.make_struct ty ;
            Reader.make_struct ty ;
          ]
        ) ty
      in
      List.map (translate_implementation ?ghost_loc) funs

    | ext when Cow.supported_extension ext ->
      let module Cow = (val Cow.make_cow ~ext : Simple_conv_intf) in
      let funs = Misc.list_concat_map (fun ty -> Cow.Struct.make_funs ty) ty
      in List.map (translate_implementation ?ghost_loc) funs

    | _unsupported_ext -> []

  let generate_definitions ~ty ?ghost_loc extensions =
    Misc.list_concat_map (generate_definitions ~ty ?ghost_loc) extensions

  let rec generate_sigs ~ty ?ghost_loc = function
    | "sexp" ->
      let sigs = Misc.list_concat_map Sexp.Sig.make_decls ty in
      List.rev_map (translate_declaration ?ghost_loc) sigs

    | "bin_write" ->
      let sigs =
        let open Binprot in
        Misc.list_concat_map
          (fun ty ->
            [ Sizer.make_sig ty ; Write.make_sig ty ; Writer.make_sig ty ])
          ty
      in
      List.rev_map (translate_declaration ?ghost_loc) sigs

    | "bin_read" ->
      let sigs =
        let open Binprot in
        Misc.list_concat_map
          (fun ty -> [ Read.make_sig ty ; Reader.make_sig ty ]) ty
      in
      List.rev_map (translate_declaration ?ghost_loc) sigs

    | "bin_io" ->
      let sigs =
        let open Binprot in
        Misc.list_concat_map
          (fun ty ->
            [
              Sizer.make_sig ty ;
              Write.make_sig ty ;
              Writer.make_sig ty ;
              Read.make_sig ty ;
              Reader.make_sig ty ;
            ])
          ty
      in
      List.rev_map (translate_declaration ?ghost_loc) sigs

    | ext when Cow.supported_extension ext ->
      let module Cow = (val Cow.make_cow ~ext : Simple_conv_intf) in
      let sigs = Misc.list_concat_map (Cow.Sig.make_decls) ty
      in List.rev_map (translate_declaration ?ghost_loc) sigs

    | _unsupported_ext -> []

  let generate_sigs ~ty ?ghost_loc extensions =
    Misc.list_concat_map (generate_sigs ~ty ?ghost_loc) extensions

end

