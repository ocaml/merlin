open Std

let { Logger.log } = Logger.for_section "signature-help"

type parameter_info =
  { label : Asttypes.arg_label;
    param_start : int;
    param_end : int;
    argument : Typedtree.expression option
  }

type application_signature =
  { function_name : string option;
    function_position : Msource.position;
    signature : string;
    parameters : parameter_info list;
    active_param : int option
  }

(* extract a properly parenthesized identifier from (expression_desc (Texp_ident
   (Longident))) *)
let extract_ident (exp_desc : Typedtree.expression_desc) =
  let rec longident ppf : Longident.t -> unit = function
    | Lident s -> Format.fprintf ppf "%s" (Misc_utils.parenthesize_name s)
    | Ldot (p, s) ->
      Format.fprintf ppf "%a.%s" longident p (Misc_utils.parenthesize_name s)
    | Lapply (p1, p2) -> Format.fprintf ppf "%a(%a)" longident p1 longident p2
  in
  match exp_desc with
  | Texp_ident (_, { txt = li; _ }, _) ->
    let ppf, to_string = Format.to_string () in
    longident ppf li;
    Some (to_string ())
  | _ -> None

(* Type variables shared across arguments should all be printed with the same
   name. [Printtyp.type_scheme] ensure that a name is unique within a given
   type, but not across different invocations. [reset] followed by calls to
   [mark_loops] and [type_sch] provide that *)
let pp_type env ppf ty =
  let module Printtyp = Type_utils.Printtyp in
  Printtyp.wrap_printing_env env ~verbosity:(Lvl 0) (fun () ->
      Printtyp.shared_type_scheme ppf ty)

let rec type_is_arrow ty =
  match Types.get_desc ty with
  | Tarrow _ -> true
  | Tlink ty -> type_is_arrow ty
  | Tpoly (ty, _) -> type_is_arrow ty
  | _ -> false

(* surround function types in parentheses *)
let pp_parameter_type env ppf ty =
  if type_is_arrow ty then Format.fprintf ppf "(%a)" (pp_type env) ty
  else pp_type env ppf ty

(* print parameter labels and types *)
let pp_parameter env label ppf ty =
  match (label : Asttypes.arg_label) with
  | Nolabel -> pp_parameter_type env ppf ty
  | Labelled l -> Format.fprintf ppf "%s:%a" l (pp_parameter_type env) ty
  | Optional l ->
    (* unwrap option for optional labels the same way as
       [Raw_compat.labels_of_application] *)
    let unwrap_option ty =
      match Types.get_desc ty with
      | Types.Tconstr (path, [ ty ], _) when Path.same path Predef.path_option
        -> ty
      | _ -> ty
    in
    Format.fprintf ppf "?%s:%a" l (pp_parameter_type env) (unwrap_option ty)

(* record buffer offsets to be able to underline parameter types *)
let print_parameter_offset ?arg:argument ppf buffer env label ty =
  let param_start = Buffer.length buffer in
  Format.fprintf ppf "%a%!" (pp_parameter env label) ty;
  let param_end = Buffer.length buffer in
  Format.pp_print_string ppf " -> ";
  Format.pp_print_flush ppf ();
  { label; param_start; param_end; argument }

(* This function preprocesses the signature and associate already assigned
   arguments to the corresponding parameter. (They should always be in the correct
   order in the typedtree, even if they are not in order in the source file.) *)
let separate_function_signature ~args (e : Typedtree.expression) =
  Type_utils.Printtyp.reset ();
  let buffer = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buffer in
  let rec separate ?(parameters = []) args ty =
    match (args, Types.get_desc ty) with
    | (_l, arg) :: args, Tarrow (label, ty1, ty2, _) ->
      let parameter =
        print_parameter_offset ppf buffer e.exp_env label ty1 ?arg
      in
      separate args ty2 ~parameters:(parameter :: parameters)
    | [], Tarrow (label, ty1, ty2, _) ->
      let parameter = print_parameter_offset ppf buffer e.exp_env label ty1 in
      separate args ty2 ~parameters:(parameter :: parameters)
    (* end of function type, print remaining type without recording offsets *)
    | _ ->
      Format.fprintf ppf "%a%!" (pp_type e.exp_env) ty;
      { function_name = extract_ident e.exp_desc;
        function_position = `Offset e.exp_loc.loc_end.pos_cnum;
        signature = Buffer.contents buffer;
        parameters = List.rev parameters;
        active_param = None
      }
  in
  separate args e.exp_type

let active_parameter_by_arg ~arg params =
  let find_by_arg = function
    | { argument = Some a; _ } when a == arg -> true
    | _ -> false
  in
  try Some (List.index params ~f:find_by_arg) with Not_found -> None

let first_unassigned_argument params =
  let positional = function
    | { argument = None; label = Asttypes.Nolabel; _ } -> true
    | _ -> false
  in
  let labelled = function
    | { argument = None; label = Asttypes.Labelled _ | Optional _; _ } -> true
    | _ -> false
  in
  try Some (List.index params ~f:positional)
  with Not_found -> (
    try Some (List.index params ~f:labelled) with Not_found -> None)

let active_parameter_by_prefix ~prefix params =
  let common = function
    | Asttypes.Nolabel -> Some 0
    | l
      when String.is_prefixed ~by:"~" prefix
           || String.is_prefixed ~by:"?" prefix ->
      Some (String.common_prefix_len (Btype.prefixed_label_name l) prefix)
    | _ -> None
  in

  let rec find_by_prefix ?(i = 0) ?longest_len ?longest_i = function
    | [] -> longest_i
    | p :: ps -> (
      match (common p.label, longest_len) with
      | Some common_len, Some longest_len when common_len > longest_len ->
        find_by_prefix ps ~i:(succ i) ~longest_len:common_len ~longest_i:i
      | Some common_len, None ->
        find_by_prefix ps ~i:(succ i) ~longest_len:common_len ~longest_i:i
      | _ -> find_by_prefix ps ~i:(succ i) ?longest_len ?longest_i)
  in
  find_by_prefix params

let is_arrow t =
  match Types.get_desc t with
  | Tarrow _ -> true
  | _ -> false

let application_signature ~prefix ~cursor = function
  | (_, Browse_raw.Expression arg)
    :: ( _,
         Expression { exp_desc = Texp_apply (({ exp_type; _ } as e), args); _ }
       )
    :: _
    when is_arrow exp_type ->
    log ~title:"application_signature" "Last arg:\n%a" Logger.fmt (fun fmt ->
        Printtyped.expression fmt arg);
    let result = separate_function_signature e ~args in
    let active_param =
      if prefix = "" && Lexing.compare_pos cursor arg.exp_loc.loc_end > 0 then begin
        (* If the cursor is placed after the last arg it means that a whitespace
           was inserted and we want to underline the next argument. *)
        log ~title:"application_signature"
          "Current cursor position is after the last argument";
        first_unassigned_argument result.parameters
      end
      else
        (* If not, we identify the argument which is being written *)
        let active_param = active_parameter_by_arg ~arg result.parameters in
        match active_param with
        | Some _ as ap -> ap
        | None -> active_parameter_by_prefix ~prefix result.parameters
    in
    Some { result with active_param }
  | (_, Expression ({ exp_type; _ } as e)) :: _ when is_arrow exp_type ->
    (* provide signature information directly after an unapplied function-type
       value *)
    let result = separate_function_signature e ~args:[] in
    let active_param = active_parameter_by_prefix ~prefix result.parameters in
    Some { result with active_param }
  | _ -> None

let prefix_of_position ~short_path source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let from =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let pos =
      let should_terminate = ref false in
      let has_seen_dot = ref false in
      let is_prefix_char c =
        if !should_terminate then false
        else
          match c with
          | 'a' .. 'z'
          | 'A' .. 'Z'
          | '0' .. '9'
          | '\''
          | '_'
          (* Infix function characters *)
          | '$'
          | '&'
          | '*'
          | '+'
          | '-'
          | '/'
          | '='
          | '>'
          | '@'
          | '^'
          | '!'
          | '?'
          | '%'
          | '<'
          | ':'
          | '~'
          | '#' -> true
          | '`' ->
            if !has_seen_dot then false
            else (
              should_terminate := true;
              true)
          | '.' ->
            has_seen_dot := true;
            not short_path
          | _ -> false
      in
      String.rfindi text ~from ~f:(fun c -> not (is_prefix_char c))
    in
    let pos =
      match pos with
      | None -> 0
      | Some pos -> pos + 1
    in
    let len = from - pos + 1 in
    let reconstructed_prefix = String.sub text ~pos ~len in
    (* if we reconstructed [~f:ignore] or [?f:ignore], we should take only
       [ignore], so: *)
    log ~title:"prefix_of_position" "%S" reconstructed_prefix;
    if
      String.is_prefix reconstructed_prefix ~prefix:"~"
      || String.is_prefix reconstructed_prefix ~prefix:"?"
    then
      match String.lsplit2 reconstructed_prefix ~on:':' with
      | Some (_, s) -> s
      | None -> reconstructed_prefix
    else reconstructed_prefix
