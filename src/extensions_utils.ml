
let ident = Ident.create "_"

let parse_sig str =
  let buf = Lexing.from_string str in
  Chunk_parser.interface Lexer.token buf

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let always, registry =
  let f = List.map
    (fun {Extensions. name; private_def; public_def; keywords; packages} ->
      name,
      (List.concat (List.map parse_sig private_def),
       List.concat (List.map parse_sig public_def),
       keywords,
       packages))
  in
  Extensions.(List.map snd (f always), f registry)

let all_extensions () = List.map fst registry
let ext_table = Hashtbl.create 5
let enabled () = Hashtbl.fold (fun name _ names -> name :: names) ext_table []
let disabled () =
  List.filter (fun name -> not (Hashtbl.mem ext_table name)) (all_extensions ())

let set_raw_extension ~enabled (name,(_,_,kw,_ as ext)) =
  Lexer.set_extension ~enabled kw;
  if enabled
  then Hashtbl.replace ext_table name ext
  else Hashtbl.remove ext_table name

let set_extension ~enabled name =
  try
    let (_,_,keywords,_) as ext = List.assoc name registry in
    set_raw_extension ~enabled (name,ext)
  with Not_found -> ()

let register_packages packages =
  let exts =
    List.filter (fun (_,(_,_,_,ps)) ->
                      List.exists (fun p -> List.mem p packages) ps)
                registry
  in
  List.iter (set_raw_extension ~enabled:true) exts

let register env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with exn -> [] in
  let enabled = always @ Hashtbl.fold (fun _ ext exts -> ext :: exts)
                          ext_table []
  in
  let fakes, tops =
    List.split
      (List.map (fun (fake,top,_,_) -> try_type fake, try_type top) enabled)
  in
  let env = Env.add_signature (List.concat tops) env in
  let env = Env.add_module ident (Types.Mty_signature (lazy (List.concat fakes))) env in
  env
