open Std

let ident = Ident.create "_"

let parse_sig str =
  let buf = Lexing.from_string str in
  Chunk_parser.interface Lexer.token buf

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let always, registry =
  let f = List.map
    ~f:(fun {Extensions. name; private_def; public_def; keywords; packages} ->
      name,
      (List.concat_map ~f:parse_sig private_def,
       List.concat_map ~f:parse_sig public_def,
       keywords,
       packages))
  in
  Extensions.(List.map ~f:snd (f always), f registry)
let all_extensions = List.map ~f:fst registry
let ext_table = Hashtbl.create 5

let list = function
  | `All ->
    all_extensions
  | `Enabled ->
    Hashtbl.fold (fun name _ names -> name :: names) ext_table []
  | `Disabled ->
    List.filter (fun name -> not (Hashtbl.mem ext_table name)) all_extensions

let parser_valid = ref (ref false)

let set_raw_extension ~enabled (name,(_,_,kw,_ as ext)) =
  Lexer.set_extension ~enabled kw;
  !parser_valid := false;
  if enabled
  then Hashtbl.replace ext_table name ext
  else Hashtbl.remove ext_table name

let set_extension ~enabled name =
  try  let ext = List.assoc name registry in
       set_raw_extension ~enabled (name,ext)
  with Not_found -> ()

let set_extensions list =
  List.iter
    (fun ext ->
       let enabled = (List.mem ext list) in
       if enabled <> Hashtbl.mem ext_table ext then
         set_extension ~enabled ext)
    all_extensions

let extensions_from_packages packages =
  List.filter_map registry
    ~f:(fun (ext,(_,_,_,ps)) ->
        if List.exists (fun p -> List.mem p packages) ps
        then Some ext
        else None)

let parser_valid () =
  if not !(!parser_valid) then
    parser_valid := ref true;
  !parser_valid

let register env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with _exn -> [] in
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
