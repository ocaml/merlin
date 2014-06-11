type t = {
  loc: Location.t;
  state: Raw_parser.state;
  token: Raw_parser.token;
}
exception Error of t

let loc t = t.loc
let classify {state; token} =
  Printf.sprintf "Syntax error (%d,%s)"
    (Obj.magic state)
    Merlin_parser.Values.(string_of_class (class_of_symbol (symbol_of_token token)))

let from parser (s,token,e) =
  let state = parser.Raw_parser.env.MenhirLib.EngineTypes.current in
  let loc = {Location. loc_start = s; loc_end = e; loc_ghost = false } in
  Error { loc; state; token }
