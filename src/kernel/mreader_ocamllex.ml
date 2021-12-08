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
    | Let_regexp _ as x -> x
    | Refill_handler action -> Refill_handler (parse local_action action)
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

let join t1 t2 = match t1, t2 with
  | T_string, _ | _, T_string -> T_string
  | _, _ -> T_char

let rec collect_names named acc = function
  | Characters -> acc, T_char
  | Epsilon | String | Eof -> acc, T_string
  | Named {Location. txt; _} ->
    begin match Std.String.Map.find_opt txt named with
      | None ->
      | Some (loc, {Location. txt=def; _}) ->
        collect_names named acc def
    end
  | Sequence (e1, e2) ->
    let acc, _ = collect_names named acc e2 in
    let acc, _ = collect_names named acc e1 in
    acc, T_string
  | Hash (e1, e2, _) ->
    let acc, _ = collect_names named acc e2 in
    let acc, _ = collect_names named acc e1 in
    acc, T_char
   | Alternative (e1, e2) ->
    let acc, t1 = collect_names named acc e2 in
    let acc, t2 = collect_names named acc e1 in
    acc, join t1 t2
  | Star e1 | Plus e1 ->
    let acc, _ = collect_names named acc e1 in
    acc, T_string
  | Bind (e1, name) ->
    let acc, t1 = collect_names named acc e1 in
    (((name, t1) :: acc), t1)

let mk_const = function
  | T_char -> Const.char ' '
  | T_string -> Const.string ""

let elaborate_clause named {pattern; action} =
  let names, _ = collect_names named [] pattern.Location.txt in
  let bind_name (name, typ) body =
    Exp.let_ Nonrecursive
      [Vb.mk (Pat.var name) (Exp.constant (mk_const typ))]
      body
  in
  List.fold_right bind_name names action

let elaborate_rule named rule =
  let clauses = List.map (elaborate_clause named) rule.clauses in
  let add_clause clause body =
    Exp.try_ clause [Exp.case (Pat.any ()) body]
  in
  let false_ =
      Exp.construct (Location.mknoloc (Longident.Lident "false")) None
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
  | Let_regexp ({Location. txt; loc}, regexp) :: rest ->
    let named = Std.String.Map.add ~key:txt ~data:(loc, regexp) named in
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

let read config source =
  let lexbuf = Lexing.from_string ~with_positions:true (Msource.text source) in
  let chunks = Ocamllex_parser.lexer_chunks Ocamllex_lexer.main lexbuf in
  elaborate_chunks Std.String.Map.empty (parse_chunks config source chunks)
