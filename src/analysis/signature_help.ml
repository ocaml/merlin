open Std

open Browse_raw

open Extend_protocol.Reader

type application_signature =
  { fun_name : string option
  ; signature : string
  ; param_offsets : (int * int) list
  ; active_param : int option
  }

(* extract a properly properly parenthesized identifier
   from (expression_desc (Texp_ident (Longident))) *)
let extract_ident (exp_desc : Typedtree.expression_desc) =
  let rec longident ppf : Longident.t -> unit = function
    | Lident s -> fprintf ppf "%s" (Completion.parenthesize_name s)
    | Ldot (p, s) -> fprintf ppf "%a.%s" longident p (Completion.parenthesize_name s)
    | Lapply (p1, p2) -> fprintf ppf "%a(%a)" longident p1 longident p2
  in
  match exp_desc with
  | Texp_ident (_, { txt = li; _ }, _) ->
    let ppf, to_string = Format.to_string () in
    longident ppf li;
    Some (to_string ())
  | _ -> None


(* Type variables shared across arguments should all be
   printed with the same name.
   [Printtyp.type_scheme] ensure that a name is unique within a given
   type, but not across different invocations.
   [reset] followed by calls to [mark_loops] and [type_sch] provide
   that *)
let pp_type env ppf ty =
  let module Printtyp = Type_utils.Printtyp in
  Printtyp.wrap_printing_env env ~verbosity:0 (fun () ->
    Printtyp.mark_loops ty;
    Printtyp.type_sch ppf ty)

(* surround function types in parentheses *)
let pp_parameter_type env ppf ty =
  match ty with
  | { Types.desc = Tarrow _; _ } as ty ->
    Format.fprintf ppf "(%a)" (pp_type env) ty
  | ty -> pp_type env ppf ty


(* print parameter labels and types *)
let pp_parameter env label ppf ty =
  match label with
  | Asttypes.Nolabel ->
    pp_parameter_type env ppf ty
  | Asttypes.Labelled l ->
    Format.fprintf ppf "%s:%a" l (pp_parameter_type env) ty
  | Asttypes.Optional l ->
    (* unwrap option for optional labels the same way as
       [Raw_compat.labels_of_application] *)
    let unwrap_option ty = match (Ctype.repr ty).Types.desc with
      | Types.Tconstr (path, [ty], _)
        when Path.same path Predef.path_option -> ty
      | _ -> ty
    in
    Format.fprintf ppf "?%s:%a" l (pp_parameter_type env) (unwrap_option ty)

(* record buffer offsets to be able to underline parameter types *)
let print_parameter_offset buffer env label ty =
  let ppf = Format.formatter_of_buffer buffer in
  let param_start = Buffer.length buffer in
  Format.fprintf ppf "%a%!" (pp_parameter env label) ty;
  let param_end = Buffer.length buffer in
  Format.pp_print_string ppf " -> ";
  Format.pp_print_flush ppf ();
  (param_start, param_end)

let application_signature = function
  (* provide signature information for applied functions *)
  | (_, Expression earg) :: (_, Expression { exp_desc =
      Texp_apply ({ exp_type = { desc = Tarrow _; _ }; _ } as efun, eargs); _ }) :: _ ->
    Type_utils.Printtyp.reset ();
    let buffer = Buffer.create 16 in
    let rec separate_signature ?(i=0) ?(param_offsets=[]) ?active_param args ty =
      match (args, ty) with
      | [], { Types.desc = Tarrow (label, ty1, ty2, _) } ->
        let offset = print_parameter_offset buffer efun.exp_env label ty1 in
        let param_offsets = offset::param_offsets in
        separate_signature args ty2 ~i:(succ i) ~param_offsets ?active_param

      | arg :: args, { Types.desc = Tarrow (label, ty1, ty2, _) } ->
        let offset = print_parameter_offset buffer efun.exp_env label ty1 in
        let param_offsets = offset::param_offsets in
        let active_param = match arg with
          | (l, Some e) when l = label && e == earg -> Some i
          | _ -> active_param
        in
        separate_signature args ty2 ~i:(succ i) ~param_offsets ?active_param

      (* end of function type, print remaining type without recording offsets *)
      | _ ->
        Format.fprintf (Format.formatter_of_buffer buffer)
          "%a%!" (pp_type efun.exp_env) ty;
        { fun_name = extract_ident efun.exp_desc
        ; signature = Buffer.contents buffer
        ; param_offsets = List.rev param_offsets
        ; active_param
        }
    in
    `Application (separate_signature eargs efun.exp_type)

  (* provide signature information directly after
     an unapplied function-type value *)
  | (_, Expression ({ exp_type = { desc = Tarrow _; _ }; _ } as e)) :: _ ->
    Type_utils.Printtyp.reset ();
    let buffer = Buffer.create 16 in
    (* sets active_param to index of first unlabelled parameter *)
    let rec separate_signature ?(i=0) ?(param_offsets=[]) ?active_param = function
      | { Types.desc = Tarrow (label, ty1, ty2, _) } ->
        let offset = print_parameter_offset buffer e.exp_env label ty1 in
        let param_offsets = offset::param_offsets in
        let active_param = match active_param, label with
          | None, Asttypes.Nolabel -> Some i
          | _ -> active_param
        in
        separate_signature ty2 ~i:(succ i) ~param_offsets ?active_param

      (* end of function type, print remaining type without recording offsets *)
      | ty ->
        Format.fprintf (Format.formatter_of_buffer buffer)
          "%a%!" (pp_type e.exp_env) ty;
        { fun_name = extract_ident e.exp_desc
        ; signature = Buffer.contents buffer
        ; param_offsets = List.rev param_offsets
        ; active_param
        }
    in
    `Application (separate_signature e.exp_type)
  | _ -> `Unknown
