(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

let const_string s = Const_string s

let rtag ?(attrs=[]) label bool lst = Rtag (label, bool, lst)

let default_loc = ref Location.none

let with_default_loc l f =
  let old = !default_loc in
  default_loc := l;
  try let r = f () in default_loc := old; r
  with exn -> default_loc := old; raise exn

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d; ptyp_loc = loc}
  let attr d _ = d

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?(loc = !default_loc) ?attrs a _ =
    let a =
      List.map (fun (name, _, ct) ->
        { pfield_desc = Pfield (name, ct) ; pfield_loc = loc }
      ) a
    in
    mk ~loc ?attrs (Ptyp_object a)
  let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b, []))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c =
    let b = match b with Closed -> false | _ -> true in (* ?? Go figure. *)
    mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))

  let extension ?loc ?attrs _ext = assert false

  let force_poly t =
    match t.ptyp_desc with
    | Ptyp_poly _ -> t
    | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d; ppat_loc = loc}
  let attr d _ = d

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = assert false
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b, true)) (* FIXME: false? *)
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
  let exception_ ?loc ?attrs a = assert false
  let extension ?loc ?attrs a = assert false
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d = {pexp_desc = d; pexp_loc = loc}
  let attr d a = d

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c =
    let b = List.map (fun { pvb_pat ; pvb_expr } -> pvb_pat, pvb_expr) b in
    mk ?loc ?attrs (Pexp_let (a, b, c))
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_function (a, b, [(c, d)]))
  let function_ ?loc ?attrs a =
    let lst =
      List.map (fun { pc_lhs ; pc_guard ; pc_rhs } ->
        let expr =
          match pc_guard with
          | None -> pc_rhs
          | Some e -> mk (Pexp_when (e, pc_rhs))
        in
        pc_lhs, expr
      ) a
    in
    mk ?loc ?attrs (Pexp_function ("", None, lst))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b =
    let b =
      List.map (fun { pc_lhs ; pc_guard ; pc_rhs } ->
        let expr =
          match pc_guard with
          | None -> pc_rhs
          | Some e -> mk (Pexp_when (e, pc_rhs))
        in
        pc_lhs, expr
      ) b
    in
    mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b =
    let b =
      List.map (fun { pc_lhs ; pc_guard ; pc_rhs } ->
        let expr =
          match pc_guard with
          | None -> pc_rhs
          | Some e -> mk (Pexp_when (e, pc_rhs))
        in
        pc_lhs, expr
      ) b
    in
    mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b, true))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e =
    let a = match a.ppat_desc with
      | Ppat_var name -> name
      | _ -> assert false
    in
    mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, Some b, None))
  let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_constraint (a, b, Some c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
  let extension ?loc ?attrs a = assert false

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc}
  let attr d _a = d

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a) (* Questionable. *)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c =
    let b = match b with
      | None -> assert false
      | Some x -> x
    in
    mk ?loc ?attrs (Pmty_functor (a, b, c))
  let with_ ?loc ?attrs a b =
    assert false (* Reconcile [with_constraint] *)
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = assert false
end

module Mod = struct
let mk ?(loc = !default_loc) ?(attrs = []) d =
  {pmod_desc = d; pmod_loc = loc}
  let attr d a = d

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg arg_ty body =
    match arg_ty with
    | None -> assert false (* not possible < 4.02 *)
    | Some arg_ty -> mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = assert false
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a =
    let { Override. pval_name ; pval_type ; pval_prim ; pval_loc } = a in
    mk ?loc (Psig_value (pval_name, { pval_type ; pval_prim ; pval_loc }))
  let type_ ?loc a = failwith "TODO" (* mk ?loc (Psig_type a) *)
  let type_extension ?loc a = assert false
  let exception_ ?loc a = failwith "TODO" (* mk ?loc (Psig_exception a) *)
  let module_ ?loc { pmd_name ; pmd_type ; _ } =
    mk ?loc (Psig_module (pmd_name, pmd_type))
  let rec_module ?loc a =
    let a =
      List.map (fun { pmd_name ; pmd_type ; _ } -> pmd_name, pmd_type) a
    in
    mk ?loc (Psig_recmodule a)
  let modtype ?loc { pmtd_name ; pmtd_type } =
    let modtype_decl =
      match pmtd_type with
      | None -> Pmodtype_abstract
      | Some m -> Pmodtype_manifest m
    in
    mk ?loc (Psig_modtype (pmtd_name, modtype_decl))
  let open_ ?loc { popen_lid; popen_override; popen_loc } =
    let loc =
      match loc with
      | None -> popen_loc
      | Some l -> l
    in
    mk ~loc (Psig_open (popen_override, popen_lid))
  let include_ ?loc a = assert false (* TODO *)
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let attribute ?loc a = assert false
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval a)
  let value ?loc a b =
    let b = List.map (fun { pvb_pat ; pvb_expr } -> pvb_pat, pvb_expr) b in
    mk ?loc (Pstr_value (a, b))
  let primitive ?loc { Override. pval_name ; pval_type ; pval_prim ; pval_loc} =
    mk ?loc (Pstr_primitive (pval_name, { pval_type ; pval_prim ; pval_loc }))
  let type_ ?loc a = failwith "TODO" (* mk ?loc (Pstr_type a) *)
  let exception_ ?loc a = failwith "TODO"
  let module_ ?loc { pmb_name ; pmb_expr } =
    mk ?loc (Pstr_module (pmb_name, pmb_expr))
  let rec_module ?loc a = failwith "TODO" (* mk ?loc (Pstr_recmodule a) *)
  let modtype ?loc { pmtd_name ; pmtd_type } =
    let modtype =
      match pmtd_type with
      | None -> assert false
      | Some m -> m
    in
    mk ?loc (Pstr_modtype (pmtd_name, modtype))
  let open_ ?loc { popen_lid; popen_override; popen_loc } =
    let loc =
      match loc with
      | None -> popen_loc
      | Some l -> l
    in
    mk ~loc (Pstr_open (popen_override, popen_lid))
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let attribute ?loc a = assert false
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcl_desc = d;
     pcl_loc = loc;
    }
  let attr d a = d

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
  let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
  let let_ ?loc ?attrs a b c =
    let b = List.map (fun { pvb_pat ; pvb_expr } -> pvb_pat, pvb_expr) b in
    mk ?loc ?attrs (Pcl_let (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcty_desc = d;
     pcty_loc = loc;
    }
  let attr d a = d

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_fun (a, b, c))
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pctf_desc = d;
     pctf_loc = loc;
    }
  let attr d a = d

  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inher a)
  let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
  let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_meth (a, b, d))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_cstr (a, b))
  let attribute ?loc a = assert false
end

(*
module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = attrs;
    }
  let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
  let attribute ?loc a = mk ?loc (Pcf_attribute a)

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)
end
*)

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(prim = []) name typ =
    { Override.
     pval_name = name;
     pval_type = typ;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?(attrs = []) name typ =
    {
     pmd_name = name;
     pmd_type = typ;
     pmd_loc = loc;
    }
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?typ name =
    {
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_loc = loc;
    }
end

module Mb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) name expr =
    {
     pmb_name = name;
     pmb_expr = expr;
     pmb_loc = loc;
    }
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(override = Fresh) lid =
    {
     popen_lid = lid;
     popen_override = override;
     popen_loc = loc;
    }
end

(*
module Incl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) mexpr =
    {
     pincl_mod = mexpr;
     pincl_loc = loc;
     pincl_attributes = attrs;
    }
end
*)

module Vb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) pat expr =
    {
     pvb_pat = pat;
     pvb_expr = expr;
     pvb_loc = loc;
    }
end

(*
module Ci = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(virt = Concrete) ?(params = [])
         name expr =
    {
     pci_virt = virt;
     pci_params = params;
     pci_name = name;
     pci_expr = expr;
     pci_loc = loc;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      name =
    {
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes = attrs;
     ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(args = []) ?res name =
    {
     pcd_name = name;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = attrs;
    }
end

(** Type extensions *)
module Te = struct
  let mk ?(attrs = []) ?(params = []) ?(priv = Public) path constructors =
    {
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_attributes = attrs;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) name kind =
    {
     pext_name = name;
     pext_kind = kind;
     pext_loc = loc;
     pext_attributes = attrs;
    }

  let decl ?(loc = !default_loc) ?(attrs = []) ?(args = []) ?res name =
    {
     pext_name = name;
     pext_kind = Pext_decl(args, res);
     pext_loc = loc;
     pext_attributes = attrs;
    }

  let rebind ?(loc = !default_loc) ?(attrs = []) name lid =
    {
     pext_name = name;
     pext_kind = Pext_rebind lid;
     pext_loc = loc;
     pext_attributes = attrs;
    }
end


module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end
*)
