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

type type_scheme = [
  | `Var   of string
  | `Arrow of Asttypes.label * type_scheme * type_scheme
  | `Named of type_scheme list * string
]

(* extend as needed *)
type ast = [
  | `Let   of binding list
  | `Fun   of string list * ast
  | `App   of ast * ast
  | `Ident of string
  | `AnyVal (* wild card ident *)
  | `Val   of string * type_scheme (* TODO: use something similar to [binding] type? *)
]
and binding = {
  ident   : string ;
  typesig : type_scheme ;
  body    : ast ;
}

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

let translate_declaration ?ghost_loc = function
  | `Val (id, sign) ->
    let pval_ty = {
      pval_type = translate_ts ?ghost_loc sign ;
      pval_prim = [] ;
      pval_loc  = default_loc ghost_loc ;
    } in
    let psig_desc = Psig_value (mkoptloc ghost_loc id, pval_ty) in
    { psig_desc ; psig_loc = default_loc ghost_loc }

let rec translate_binding ?ghost_loc { ident ; typesig ; body } =
  let pat = {
    ppat_desc = Ppat_var (mkoptloc ghost_loc ident) ;
    ppat_loc = default_loc ghost_loc ;
  }
  in
  let typesig_opt = Some (translate_ts ?ghost_loc typesig) in
  let body = translate_to_expr ?ghost_loc body in
  let pexp = {
    pexp_desc = Pexp_constraint (body, typesig_opt, None) ;
    pexp_loc = default_loc ghost_loc ;
  }
  in
  (pat, pexp)

and translate_to_str ?ghost_loc = function
  | `Let lst ->
    let p = Pstr_value (Asttypes.Nonrecursive, List.map translate_binding lst) in
    { pstr_desc = p ; pstr_loc = default_loc ghost_loc }

and translate_to_expr ?ghost_loc = function
  | `Let _ -> failwith "not allowed at this level"
  | `Val _ -> failwith "clearly not allowed here" (* TODO: refine [ast] *)
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
      (translate_to_expr ?ghost_loc body)
  | `App (f, x) ->
    app (translate_to_expr ?ghost_loc f) (translate_to_expr ?ghost_loc x)
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


module Sexp : sig
  type ty = string Location.loc * Parsetree.type_declaration

  module Struct : sig
    val make_funs : ty -> [ `Let of binding list ]
  end

  module Sig : sig
    val make_decls : ty -> [ `Val of string * type_scheme ] list
  end
end = struct
  type ty = string Location.loc * Parsetree.type_declaration

  let t = `Named ([], "Sexplib.Sexp.t")

  module TypeSig = struct
    let mk_arrow x y = `Arrow ("", x, y)

    let sexp_of params ty =
      let params = format_params ~f:(fun v -> `Var v) params in
      List.fold_right (fun var acc -> mk_arrow (mk_arrow var t) acc) params
        (mk_arrow (`Named (params, ty)) t)

    let of_sexp params ty =
      let params = format_params ~f:(fun v -> `Var v) params in
      List.fold_right (fun var acc -> mk_arrow (mk_arrow t var) acc) params
        (mk_arrow t (`Named (params, ty)))
  end

  module Struct = struct
    let sexp_of_ (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let args = format_params ~f:(fun x -> "sexp_of_" ^ x) type_infos.ptype_params
      in
      {
        ident = "sexp_of_" ^ ty ;
        typesig = TypeSig.sexp_of type_infos.ptype_params ty;
        body = mk_fun ~args ;
      }

    let _of_sexp (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let args = format_params ~f:(fun x -> x ^ "_of_sexp") type_infos.ptype_params
      in
      {
        ident = ty ^ "_of_sexp" ;
        typesig = TypeSig.of_sexp type_infos.ptype_params ty;
        body = mk_fun ~args ;
      }

    let make_funs ty = `Let [ sexp_of_ ty ; _of_sexp ty ]
  end

  module Sig = struct
    let sexp_of_ (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let typesig = TypeSig.sexp_of type_infos.ptype_params ty in
      `Val ("sexp_of_" ^ ty, typesig)

    let _of_sexp (located_name, type_infos) =
      let ty = located_name.Location.txt in
      let typesig = TypeSig.of_sexp type_infos.ptype_params ty in
      `Val (ty ^ "_of_sexp", typesig)


    let make_decls ty = [ sexp_of_ ty ; _of_sexp ty ]
  end
end

module Binprot = struct

  let make_struct ~prefix ~typesig (located_name, ty_infos) =
    let tyname = located_name.Location.txt in
    let args = format_params ~f:(fun x -> prefix ^ x) ty_infos.ptype_params in
    { ident = prefix ^ tyname ; typesig = typesig ; body = mk_fun ~args }

  module Sizer = struct
    let int = `Named ([], "int")

    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> x) ty_infos.ptype_params in
      List.fold_right (fun v acc -> `Arrow ("", `Arrow ("", `Var v, int), acc)) params
        (`Arrow ("", `Named (List.map (fun x -> `Var x) params, name.Location.txt), int))

    let make_struct ty =
      let typesig = typesig ty in
      make_struct ~prefix:"bin_size_" ~typesig ty

    let make_sig ty =
      let tyname = (fst ty).Location.txt in
      let typesig = typesig ty in
      `Val ("bin_size_" ^ tyname, typesig)
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

    let make_struct ty =
      let typesig = typesig ty in
      make_struct ~prefix:"bin_write_" ~typesig ty

    let make_sig ty =
      let tyname = (fst ty).Location.txt in
      let typesig = typesig ty in
      `Val ("bin_write_" ^ tyname, typesig)
  end

  module Writer = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.writer"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.writer"))

    let make_struct ty =
      let typesig = typesig ty in
      make_struct ~prefix:"bin_writer_" ~typesig ty

    let make_sig ty =
      let tyname = (fst ty).Location.txt in
      let typesig = typesig ty in
      `Val ("bin_writer_" ^ tyname, typesig)
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

    let make_struct ty =
      let typesig = typesig ty in
      make_struct ~prefix:"bin_read_" ~typesig ty

    let make_sig ty =
      let tyname = (fst ty).Location.txt in
      let typesig = typesig ty in
      `Val ("bin_read_" ^ tyname, typesig)
  end

  module Reader = struct
    let typesig (name, ty_infos) =
      let params = format_params ~f:(fun x -> `Var x) ty_infos.ptype_params in
      List.fold_right
        (fun param acc -> `Arrow ("", `Named ([param], "Bin_prot.Type_class.reader"), acc))
        params
        (`Named ([`Named (params, name.Location.txt)], "Bin_prot.Type_class.reader"))

    let make_struct ty =
      let typesig = typesig ty in
      make_struct ~prefix:"bin_reader_" ~typesig ty

    let make_sig ty =
      let tyname = (fst ty).Location.txt in
      let typesig = typesig ty in
      `Val ("bin_reader_" ^ tyname, typesig)
  end
end

module TypeWith = struct
  let rec generate_definitions ~ty ?ghost_loc = function
    | "sexp" :: rest ->
      let funs = List.map Sexp.Struct.make_funs ty in
      let ast = List.map (translate_to_str ?ghost_loc) funs in
      ast @ (generate_definitions ~ty ?ghost_loc rest)

    | "bin_write" :: rest ->
      let funs =
        let open Binprot in
        List.map (fun ty ->
          `Let [ Sizer.make_struct ty ; Write.make_struct ty ; Writer.make_struct ty ]
        ) ty
      in
      let ast = List.map (translate_to_str ?ghost_loc) funs in
      ast @ (generate_definitions ~ty ?ghost_loc rest)

    | "bin_read" :: rest ->
      let funs =
        let open Binprot in
        List.map (fun ty -> `Let [ Read.make_struct ty ; Reader.make_struct ty ]) ty
      in
      let ast = List.map (translate_to_str ?ghost_loc) funs in
      ast @ (generate_definitions ~ty ?ghost_loc rest)


    | "bin_io" :: rest ->
      let funs =
        let open Binprot in
        List.map (fun ty ->
          `Let [
            Sizer.make_struct ty ;
            Write.make_struct ty ;
            Writer.make_struct ty ;
            Read.make_struct ty ;
            Reader.make_struct ty ;
        ]
        ) ty
      in
      let ast = List.map (translate_to_str ?ghost_loc) funs in
      ast @ (generate_definitions ~ty ?ghost_loc rest)

    | _  :: rest -> generate_definitions ~ty ?ghost_loc rest

    | [] -> []

  let rec generate_sigs ~ty ?ghost_loc = function
    | "sexp" :: rest ->
      let sigs = List.concat (List.map Sexp.Sig.make_decls ty) in
      let ast = List.rev_map (translate_declaration ?ghost_loc) sigs in
      ast @ (generate_sigs ~ty ?ghost_loc rest)

    | "bin_write" :: rest ->
      let sigs =
        let open Binprot in
        List.concat (
          List.map (fun ty ->
            [ Sizer.make_sig ty ; Write.make_sig ty ; Writer.make_sig ty ]
          ) ty
        )
      in
      let ast = List.rev_map (translate_declaration ?ghost_loc) sigs in
      ast @ (generate_sigs ~ty ?ghost_loc rest)

    | "bin_read" :: rest ->
      let sigs =
        let open Binprot in
        List.concat (
          List.map (fun ty -> [ Read.make_sig ty ; Reader.make_sig ty ]) ty
        )
      in
      let ast = List.rev_map (translate_declaration ?ghost_loc) sigs in
      ast @ (generate_sigs ~ty ?ghost_loc rest)


    | "bin_io" :: rest ->
      let sigs =
        let open Binprot in
        List.concat (
          List.map (fun ty ->
            [
              Sizer.make_sig ty ;
              Write.make_sig ty ;
              Writer.make_sig ty ;
              Read.make_sig ty ;
              Reader.make_sig ty ;
            ]
          ) ty
        )
      in
      let ast = List.rev_map (translate_declaration ?ghost_loc) sigs in
      ast @ (generate_sigs ~ty ?ghost_loc rest)


    | _ :: rest -> generate_sigs ~ty ?ghost_loc rest

    | [] -> []
end
