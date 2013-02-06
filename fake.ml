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

type type_scheme = [
  | `Var   of string
  | `Arrow of type_scheme * type_scheme
  | `Named of type_scheme list * string
]

(* extend as needed *)
type ast = [
  | `Let   of binding list
  | `Fun   of string list * ast
  | `App   of ast * ast
  | `Ident of string
]
and binding = {
  ident   : string ;
  typesig : type_scheme ;
  body    : ast ;
}

let rec translate_ts ?ghost_loc = function
  | `Var ident ->
    { ptyp_desc = Ptyp_var ident ; ptyp_loc = default_loc ghost_loc }
  | `Arrow (a, b) ->
    let a = translate_ts a in
    let b = translate_ts b in
    { ptyp_desc = Ptyp_arrow("", a, b) ; ptyp_loc = default_loc ghost_loc }
  | `Named (params, id) ->
    let id = Longident.parse id in
    let params = List.map translate_ts params in
    {
      ptyp_desc = Ptyp_constr (mkoptloc ghost_loc id, params) ;
      ptyp_loc = default_loc ghost_loc ;
    }

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

let prim_ident prim = Longident.parse ("_." ^ prim)
let prim prim = {
  pexp_desc = Pexp_ident (Location.mknoloc (prim_ident prim));
  pexp_loc = Location.none
}

module Lwt = struct
  let un_lwt = prim "Lwt.un_lwt"
  let to_lwt = prim "Lwt.to_lwt"
  let in_lwt = prim "Lwt.in_lwt"
  let unit_lwt = prim "Lwt.unit_lwt"
  let un_stream = prim "Lwt.un_stream"
  let finally' = prim "Lwt.finally'"
  let raise_lwt' = prim_ident "Lwt.raise_lwt'"
end

module Sexp : sig
  val make_funs :
    string Location.loc * Parsetree.type_declaration
    -> [ `Let of binding list ]
end = struct
  let t = `Named ([], "Sexplib.Sexp.t")

  let format_params ~format_arg lst =
    List.fold_right
      (fun param (args, params) ->
        match param with
        | None -> (format_arg "_") :: args, `Var "_" :: params
        | Some id -> id.Location.txt :: args, `Var id.Location.txt :: params)
      lst
      (["x"], [])

  let mk_fun ~args = `Fun (args, `App (`Ident "Obj.magic", `Ident "x"))

  let sexp_of_ (located_name, type_infos) =
    let ty = located_name.Location.txt in
    let args, params =
      format_params ~format_arg:(fun x -> "sexp_of_" ^ x) type_infos.ptype_params
    in
    let typesig =
      List.fold_right (fun var acc -> `Arrow (`Arrow (var, t), acc)) params
        (`Arrow (`Named (params, ty), t))
    in
    {
      ident = "sexp_of_" ^ ty ;
      typesig ;
      body = mk_fun ~args ;
    }

  let _of_sexp (located_name, type_infos) =
    let ty = located_name.Location.txt in
    let args, params =
      format_params ~format_arg:(fun x -> x ^ "_of_sexp") type_infos.ptype_params
    in
    let typesig =
      List.fold_right (fun var acc -> `Arrow (`Arrow (t, var), acc)) params
        (`Arrow (t, `Named (params, ty)))
    in
    {
      ident = ty ^ "_of_sexp" ;
      typesig ;
      body = mk_fun ~args ;
    }

  let make_funs ty = `Let [ sexp_of_ ty ; _of_sexp ty ]
end

module TypeWith = struct
  let rec generate_definitions ~ty ?ghost_loc = function
    | "sexp" :: rest ->
      let funs = List.map Sexp.make_funs ty in
      let ast = List.map (translate_to_str ?ghost_loc) funs in
      ast @ (generate_definitions ~ty ?ghost_loc rest)
    | _  :: rest -> generate_definitions ~ty ?ghost_loc rest
    | [] -> []
end
