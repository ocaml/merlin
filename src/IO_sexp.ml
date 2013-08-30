open Sexplib.Std

let rec sexp_of_json = 
  let open Sexplib.Type in
  let assoc_item (a,b) = List [Atom a; sexp_of_json b] in
  function
  | `Null -> Atom "null"
  | `Bool true -> Atom "true"
  | `Bool false -> Atom "false"
  | `Int i -> List [Atom "i"; sexp_of_int i]
  | `Float f -> List [Atom "f"; sexp_of_float f]
  | `String s -> List [Atom "s"; Atom s]
  | `Assoc lst -> List (Atom "assoc" :: (List.map assoc_item lst))
  | `List lst -> List (Atom "list" :: List.map sexp_of_json lst)

let rec json_of_sexp = 
  let fail msg sexp = 
    IO.protocol_failure 
      (msg ^ ", got: \n" ^ Sexplib.Sexp.to_string_hum sexp)
  in
  let open Sexplib.Type in
  let assoc_item = function
    | List [Atom a; b] -> (a, json_of_sexp b)
    | sexp -> fail "expecting association (key value)" sexp
  in
  function
  | Atom "null"  -> `Null
  | Atom "true"  -> `Bool true
  | Atom "false" -> `Bool false
  | List [Atom "i"; i] -> `Int (int_of_sexp i)
  | List [Atom "f"; f] -> `Float (float_of_sexp f)
  | List [Atom "s"; Atom s] -> `String s
  | List (Atom "assoc" :: assocs) -> `Assoc (List.map assoc_item assocs)
  | List (Atom "list" :: items) -> `List (List.map json_of_sexp items)
  | sexp -> fail "expecting sexp-encoded json object" sexp

let sexp_make ~input ~output =
  let input = Lexing.from_channel input in
  let input' = Stream.from (fun _ -> 
      Misc.may_map json_of_sexp Sexplib.(Parser.sexp_opt Lexer.main input))
  in
  let buf = Buffer.create 8192 in
  let output json =
    let sexp = sexp_of_json json in
    Sexplib.Sexp.to_buffer_mach ~buf sexp;
    Buffer.add_char buf '\n';
    Buffer.output_buffer output buf;
    flush output;
    if Buffer.length buf > 100_000
    then Buffer.reset buf
    else Buffer.clear buf
  in
  input', output
  
let () = IO.register_protocol
      ~name:"sexp"
      ~desc:"Simple encoding of json over sexpr"
      sexp_make
