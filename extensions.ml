let ident = Ident.create "_"

let parse_sig str =
  let buf = Lexing.from_string str in
  Chunk_parser.interface Lexer.token buf

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let ext_lwt =
  parse_sig
  "module Lwt : sig
    val un_lwt : 'a Lwt.t -> 'a
    val in_lwt : 'a Lwt.t -> 'a Lwt.t
    val to_lwt : 'a -> 'a Lwt.t
    val finally' : 'a Lwt.t -> unit Lwt.t -> 'a Lwt.t
    val un_stream : 'a Lwt_stream.t -> 'a
    val unit_lwt : unit Lwt.t -> unit Lwt.t
  end",
  parse_sig
    "val (>>) : unit Lwt.t -> 'a Lwt.t -> 'a Lwt.t
     val raise_lwt : exn -> 'a Lwt.t"

let ext_any =
  parse_sig
  "module Any : sig
    val val' : 'a  
  end",
  []
let registry = [ext_lwt;ext_any]                       

let register env =
  (* Log errors ? *)
  let try_type sg' = try type_sig env sg' with exn -> [] in
  let fakes, tops =
    List.split (List.map (fun (fake,top) -> try_type fake, try_type top) registry)
  in
  let env = Env.add_signature (List.concat tops) env in
  let env = Env.add_module ident (Types.Mty_signature (List.concat fakes)) env in
  env
