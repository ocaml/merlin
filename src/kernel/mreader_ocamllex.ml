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

let global_action = Parser_raw.Incremental.implementation
let local_action = Parser_raw.Incremental.parse_expression

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
    Mreader_parser.result
      (parse config warnings keywords source entry loc)
  in
  let parse_clause {pattern; action} =
    {pattern; action = parse local_action action}
  in
  let parse_rule rule =
    {rule with clauses = List.map parse_clause rule.clauses}
  in
  let parse_chunk = function
    | Action action -> Action (parse global_action action)
    | Rule rule -> Rule (parse_rule rule)
    | And_rule rule -> And_rule (parse_rule rule)
    | Refill_handler action -> Refill_handler (parse local_action action)
    | Let_regexp _ | Syntax_error _ as x -> x
  in
  List.map parse_chunk chunks

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

let join t1 t2 = match t1, t2 with
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
}

let add_named named def =
  let defs, index = !named in
  named := (def :: defs, Std.String.Map.add ~key:def.name ~data:def index)

let find_named named name =
  let _, index = !named in
  Std.String.Map.find_opt name index

let type_regexp named bindings errors =
  let rec visit = function
    | Characters -> T_char
    | Epsilon | String | Eof -> T_string
    | Named {Location. txt; loc} ->
      begin match find_named named txt with
        | None ->
          push_front errors
            (mk_error loc ("Regexp `" ^ txt ^ "' has not been defined"));
          add_named named {name=txt; location=loc; used=true; typ=T_unknown};
          T_unknown
        | Some named ->
          named.used <- true;
          named.typ
      end
    | Sequence (e1, e2) ->
      let _ = visit e2 in
      let _ = visit e1 in
      T_string
    | Hash (e1, e2, loc) ->
      let t2 = visit e2 in
      let t1 = visit e1 in
      begin match t1, t2 with
        | T_string, _ | _, T_string ->
          push_front errors
            (mk_error loc "Operator # expects characters sets");
        | _ -> ()
      end;
      T_char
    | Alternative (e1, e2) ->
      let t2 = visit e2 in
      let t1 = visit e1 in
      join t1 t2
    | Star e1 | Plus e1 ->
      let _ = visit e1 in
      T_string
    | Bind (e1, name) ->
      let t1 = visit e1 in
      begin match bindings with
        | Some bindings -> push_front bindings (name, t1)
        | None ->
          let loc = name.loc in
          push_front errors
            (mk_error loc "Operator `as' cannot be used in regexp definitions")
      end;
      t1
  in
  visit

let false_ = Exp.construct (Location.mknoloc (Longident.Lident "false")) None

let mk_const = function
  | T_char -> Ast_helper.Exp.constant (Const.char ' ')
  | T_string -> Ast_helper.Exp.constant (Const.string "")
  | T_unknown -> Ast_helper.Exp.assert_ false_

let elaborate_clause named {pattern; action} =
  let names = ref [] in
  let errors = ref [] in
  let _ = type_regexp named (Some names) errors pattern.Location.txt in
  let bind_error ext body =
    Exp.sequence (Exp.extension ext) body
  in
  let bind_name (name, typ) body =
    Exp.let_ Nonrecursive [Vb.mk (Pat.var name) (mk_const typ)] body
  in
  List.fold_right bind_error !errors
    (List.fold_right bind_name !names action)

let elaborate_rule named rule =
  let clauses = List.map (elaborate_clause named) rule.clauses in
  let add_clause clause body =
    Exp.try_ clause [Exp.case (Pat.any ()) body]
  in
  let body = List.fold_right add_clause clauses (Exp.assert_ false_) in
  let bind_arg arg body =
    Exp.fun_ Asttypes.Nolabel None (Pat.var arg) body
  in
  let lexing_lexbuf_ =
    Location.mknoloc Longident.(Ldot (Lident "Lexing", "lexbuf"))
  in
  let body =
    Exp.fun_ Asttypes.Nolabel None
      (Pat.constraint_
         (Pat.var (Location.mknoloc "lexbuf"))
         (Typ.constr lexing_lexbuf_ []))
      body
  in
  Vb.mk (Pat.var rule.name) (List.fold_right bind_arg rule.args body)

let rec elaborate_chunks named = function
  | [] -> []
  | Let_regexp (name, regexp) :: rest ->
    let errors = ref [] in
    let typ = type_regexp named None errors regexp.txt in
    let def = {
      name = name.txt;
      location = name.loc;
      used = false;
      typ
    } in
    add_named named def;
    elaborate_chunks named rest
  | Refill_handler _ :: rest ->
    elaborate_chunks named rest
  | Action action :: rest ->
    action @ elaborate_chunks named rest
  | (Rule rule | And_rule rule) :: rest ->
    let rules, rest = collect_rules rest in
    let rules = rule :: rules in
    Ast_helper.Str.value Asttypes.Recursive
      (List.map (elaborate_rule named) rules)
    :: elaborate_chunks named rest
  | Syntax_error loc :: rest ->
    Ast_helper.Str.extension (mk_error loc "Syntax error")
    :: elaborate_chunks named rest

let read config source =
  let lexbuf = Lexing.from_string ~with_positions:true (Msource.text source) in
  let chunks = Ocamllex_parser.lexer_chunks Ocamllex_lexer.main lexbuf in
  let named = ref ([], Std.String.Map.empty) in
  let result = elaborate_chunks named (parse_chunks config source chunks) in
  let warn_unused def result =
    if def.used then result else (
      Ast_helper.Str.extension
        (mk_error def.location ("Regexp `" ^ def.name ^ "' is never used"))
      :: result
    )
  in
  List.fold_right warn_unused (fst !named) result

