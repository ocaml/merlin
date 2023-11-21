type info = {
  name: string;
  description: string;
  example: string;
  documentation: string;
}

let node_infos : info list = [
  { name = "type_declaration"; description = "A type declaration node"; example = "let t = .."; documentation = "https://v2.ocaml.org/manual/"; };
  { name = "type_kind"; description = "A type kind node"; example = "let t = .."; documentation = "https://v2.ocaml.org/manual/"; };
  { name = "constructor_declaration"; description = "A constructor declaration node."; example = "let t = .."; documentation = "https://ocaml.org/manual/"; };
]


let get_syntax_doc name =
  List.find_opt (fun info -> info.name = name) node_infos