let ident = Ident.create "_"

let parse_sig str =
  let buf = Lexing.from_string str in
  Chunk_parser.interface Lexer.token buf

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let registry = 
  List.map (fun (name,fake,top) ->
    name,
    List.concat (List.map parse_sig fake),
    List.concat (List.map parse_sig top)
  )
  Extensions.registry

let register env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with exn -> [] in
  let fakes, tops =
    List.split (List.map (fun (_,fake,top) -> try_type fake, try_type top) registry)
  in
  let env = Env.add_signature (List.concat tops) env in
  let env = Env.add_module ident (Types.Mty_signature (List.concat fakes)) env in
  env
