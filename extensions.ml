let ident = Ident.create "_"

let parse_sig str =
  let buf = Lexing.from_string str in
  Chunk_parser.interface Outline_lexer.token buf

let type_sig env sg =
  let sg = Typemod.transl_signature env sg in
  sg.Typedtree.sig_type

let ext_lwt = parse_sig
  "module Lwt : sig
    val raise_lwt' : 'a -> 'b Lwt.t
    val un_lwt : 'a Lwt.t -> 'a
    val in_lwt : 'a Lwt.t -> 'a Lwt.t
    val to_lwt : 'a -> 'a Lwt.t
    val finally' : 'a Lwt.t -> unit Lwt.t -> 'a Lwt.t
    val un_stream : 'a Lwt_stream.t -> 'a
    val unit_lwt : unit Lwt.t -> unit Lwt.t
  end"

let registry = [ext_lwt]                       

let register env =
  let sg = List.concat (List.map (fun sg' -> try type_sig env sg' with exn -> (*FIXME:LOG ME*) []) registry) in
  Env.add_module ident (Types.Mty_signature sg) env
