open Std
open Ocamllex_syntax

let parse config warnings keywords source entry
    {Location. loc_start; loc_end; _}
  =
  let startp = loc_start.pos_cnum and endp = loc_end.pos_cnum in
  let lexer =
    Mreader_lexer.make ~offset:loc_start warnings keywords config
      (Msource.sub source ~offset:startp ~length:(endp - startp))
  in
  Mreader_parser.make warnings lexer entry

(* Checks:
   - all named regexps are bound by a "let"
   - named regexps don't contain "as"
   - floating action at the beginning and at the end, not in the middle
   - only the first rule starts with rule, other ones with and
   - refill handler only happens at the beginning
*)

let parse_chunks config source chunks =
  let warnings = Mconfig.(config.ocaml.warnings) in
  let keywords = Extension.keywords Mconfig.(config.merlin.extensions) in
  let parse entry loc =
    match
      Mreader_parser.result
        (parse config warnings keywords source entry loc)
    with
    | result -> Ok result
    | exception exn ->
      Logger.log ~section:"ocamllex" ~title:"exception while parsing"
        "%a" Logger.exn exn;
      Error loc;
  in
  let parse_local_action loc =
    match parse Parser_raw.Incremental.parse_optional_expression loc with
    | Ok (Some exp) -> Ok exp
    | Ok None ->
      Ok (Ast_helper.Exp.construct ~loc
            {Location.loc; txt=Longident.Lident "()"} None)
    | Error _ as result -> result
  in
  let parse_clause {pattern; action} =
    {pattern; action = parse_local_action action}
  in
  let parse_rule rule =
    {rule with clauses = List.map ~f:parse_clause rule.clauses}
  in
  let parse_chunk = function
    | Action action ->
      Action (parse Parser_raw.Incremental.implementation action)
    | Rule rule -> Rule (parse_rule rule)
    | And_rule rule -> And_rule (parse_rule rule)
    | Refill_handler action -> Refill_handler (parse_local_action action)
    | Let_regexp _ | Syntax_error _ as x -> x
  in
  List.map ~f:parse_chunk chunks

let rec collect_rules = function
  | And_rule rule :: rest ->
    let rules, rest = collect_rules rest in
    (rule :: rules), rest
  | rest -> [], rest

open Ast_helper

type typ =
  | T_char
  | T_string
  | T_unknown
  | T_option of typ

let t_option = function
  | T_option _ as t -> t
  | t -> T_option t

let rec join t1 t2 = match t1, t2 with
  | T_option t1, t2 -> t_option (join t1 t2)
  | t1, T_option t2 -> t_option (join t1 t2)
  | T_unknown, x | x, T_unknown -> x
  | T_char, T_char -> T_char
  | T_string, _ | _, T_string -> T_string

let mk_error loc msg : Parsetree.extension =
  { loc; txt = "ocaml.error" },
  PStr [Str.eval (Exp.constant (Pconst_string (msg, loc, None)))]

let mk_warning loc msg : Parsetree.attribute =
  Ast_mapper.attribute_of_warning loc msg

let push_front xs x =
  xs := x :: !xs

type named_regexp = {
  name: string;
  location: Location.t;
  mutable used: bool;
  typ: typ;
  bindings: typ Location.loc String.Map.t;
}

let add_named named def =
  let defs, index = !named in
  named := (def :: defs, String.Map.add ~key:def.name ~data:def index)

let find_named named name =
  let _, index = !named in
  String.Map.find_opt name index

let type_regexp named errors =
  let rec visit = function
    | Characters -> (String.Map.empty, T_char)
    | Epsilon | String | Eof -> (String.Map.empty, T_string)
    | Named {Location. txt; loc} ->
      begin match find_named named txt with
        | None ->
          push_front errors
            (mk_error loc ("Regexp `" ^ txt ^ "' has not been defined"));
          add_named named { name=txt; location=loc; used=true;
                           typ=T_unknown; bindings=String.Map.empty };
          (String.Map.empty, T_unknown)
        | Some named ->
          named.used <- true;
          (named.bindings, named.typ)
      end
    | Sequence (e1, e2) ->
      let b2, _t2 = visit e2 in
      let b1, _t1 = visit e1 in
      let bindings =
        String.Map.union ~f:(fun name t1 t2 ->
            push_front errors
              (mk_error t2.Location.loc
                 ("Capture `" ^ name ^ "' is already defined"));
            Some t1
          ) b1 b2
      in
      (bindings, T_string)
    | Hash (e1, e2, loc) ->
      let b2, t2 = visit e2 in
      let b1, t1 = visit e1 in
      begin match t1, t2 with
        | (T_string | T_option _), _
        | _, (T_string | T_option _) ->
          push_front errors
            (mk_error loc "Operator # expects characters sets");
        | _ -> ()
      end;
      begin match
          Option.plus
            (String.Map.choose_opt b1)
            (String.Map.choose_opt b2)
        with
        | None -> ()
        | Some (_, {Location. loc; _}) ->
          push_front errors
            (mk_error loc "Captures are not allowed under #");
      end;
      (String.Map.empty, T_char)
    | Alternative (e1, e2) ->
      let b2, t2 = visit e2 in
      let b1, t1 = visit e1 in
      let merge name t1 t2 =
        Some (
          match t1, t2 with
          | Some t, None | None, Some t ->
            {t with Location.txt = t_option t.Location.txt}
          | Some t1, Some t2 ->
            {Location.
              loc = t1.Location.loc;
              txt = join t1.txt t2.txt}
          | None, None -> assert false
        )
      in
      (String.Map.merge ~f:merge b1 b2, join t1 t2)
    | Star e1 ->
      let b1, _ = visit e1 in
      let optional t = {t with Location.txt = t_option t.Location.txt} in
      (String.Map.map ~f:optional b1, T_string)
    | Plus e1 ->
      let b1, _ = visit e1 in
      (b1, T_string)
    | Bind (e1, name) ->
      let b1, t1 = visit e1 in
      let b1 =
        String.Map.add ~key:name.txt ~data:{Location.txt=t1; loc=name.loc} b1
      in
      (b1, t1)
  in
  visit

let false_ = Exp.construct (Location.mknoloc (Longident.Lident "false")) None
let some_ x = Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some x)

let rec mk_const = function
  | T_char -> Ast_helper.Exp.constant (Const.char ' ')
  | T_string -> Ast_helper.Exp.constant (Const.string "")
  | T_unknown -> Ast_helper.Exp.assert_ false_
  | T_option t -> some_ (mk_const t)

let elaborate_clause named {pattern; action} =
  let errors = ref [] in
  let bindings, _ = type_regexp named errors pattern.Location.txt in
  let bind_error ext body =
    Exp.sequence (Exp.extension ext) body
  in
  let bind_name ~key:name ~data:{Location. loc; txt=typ} body =
    Exp.let_ Nonrecursive
      [Vb.mk (Pat.var {Location. txt=name; loc}) (mk_const typ)] body
  in
  let action = match action with
    | Ok action -> action
    | Error loc -> Ast_helper.Exp.extension (mk_error loc "Syntax error")
  in
  List.fold_right ~f:bind_error !errors
    ~init:(Std.String.Map.fold ~f:bind_name bindings ~init:action)

let lexing_lexbuf_ =
  Typ.constr
    (Location.mknoloc Longident.(Ldot (Lident "Lexing", "lexbuf"))) []

let elaborate_rule named rule =
  let clauses = List.map ~f:(elaborate_clause named) rule.clauses in
  let add_clause clause body =
    Exp.try_ clause [Exp.case (Pat.any ()) body]
  in
  let body = List.fold_right ~f:add_clause clauses ~init:(Exp.assert_ false_) in
  let bind_arg arg body =
    Exp.fun_ Asttypes.Nolabel None (Pat.var arg) body
  in
  let body =
    Exp.fun_ Asttypes.Nolabel None
      (Pat.constraint_ (Pat.var (Location.mknoloc "lexbuf")) lexing_lexbuf_)
      body
  in
  Vb.mk (Pat.var rule.name) (List.fold_right ~f:bind_arg rule.args ~init:body)

let rec elaborate_chunks named : Ocamllex_syntax.parsed_chunk list -> _ =
  function
  | [] -> []
  | Let_regexp (name, regexp) :: rest ->
    let errors = ref [] in
    let bindings, typ = type_regexp named errors regexp.txt in
    let def = {
      name = name.txt;
      location = name.loc;
      used = false;
      typ;
      bindings;
    } in
    add_named named def;
    elaborate_chunks named rest
  | Refill_handler action :: rest ->
    begin match action with
      | Ok exp ->
        let lexbuf_arrow =
          Typ.arrow Asttypes.Nolabel lexing_lexbuf_ (Typ.any ())
        in
        Ast_helper.Str.value Asttypes.Nonrecursive [
          Ast_helper.Vb.mk
            (Ast_helper.Pat.constraint_
               (Ast_helper.Pat.any ())
               (Typ.arrow Asttypes.Nolabel lexbuf_arrow lexbuf_arrow))
            exp
        ]
      | Error loc ->
        Ast_helper.Str.extension (mk_error loc "Syntax error")
    end :: elaborate_chunks named rest
  | Action action :: rest ->
    begin match action with
      | Ok action -> action
      | Error loc ->
        [Ast_helper.Str.extension (mk_error loc "Syntax error")]
    end @ elaborate_chunks named rest
  | (Rule rule | And_rule rule) :: rest ->
    let rules, rest = collect_rules rest in
    let rules = rule :: rules in
    Ast_helper.Str.value Asttypes.Recursive
      (List.map ~f:(elaborate_rule named) rules)
    :: elaborate_chunks named rest
  | Syntax_error loc :: rest ->
    Ast_helper.Str.extension (mk_error loc "Syntax error")
    :: elaborate_chunks named rest

let read config source =
  let lexbuf = Lexing.from_string ~with_positions:true (Msource.text source) in
  let chunks = Ocamllex_parser.lexer_chunks Ocamllex_lexer.main lexbuf in
  let named = ref ([], String.Map.empty) in
  let result = elaborate_chunks named (parse_chunks config source chunks) in
  let warn_unused def result =
    if def.used then result else (
      Ast_helper.Str.extension
        (mk_error def.location ("Regexp `" ^ def.name ^ "' is never used"))
      :: result
    )
  in
  List.fold_right ~f:warn_unused (fst !named) ~init:result

