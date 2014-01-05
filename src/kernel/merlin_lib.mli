open Misc

(* FIXME: Right now, flags are parsed by Merlin_lib, this should be moved
   elsewhere in frontend *)
val chosen_protocol : string option

(* Stateful parts:
   - typer snapshot root, done Buffer.btype
   - env cache, done Buffer.env
   - lexer keywords, done Lexer
   - compiler path, done Project
   - compiler flags, todo
*)

module Lexer: sig
  type keywords = Raw_lexer.keywords

  type t =
    | Valid of Lexing.position * Raw_parser.token * Lexing.position
    | Error of Raw_lexer.error * Location.t

  (** Create a new lexer *)
  val empty: filename:string -> t History.t

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  val start: keywords -> t History.t -> Lexing.position * (Lexing.lexbuf -> t History.t)
end

module Parser : module type of Merlin_parser

(* Project configuration *)
module Project : sig
  type t

  (** Create a new project *)
  val create: unit -> t

  (* Current buffer path *)
  val set_local_path : t -> string list -> unit

  (* Project-wide configuration *)
  val set_dot_merlin : t -> Dot_merlin.config option -> [`Ok | `Failures of (string * exn) list]

  (* Config override by user *)
  module User : sig
    val reset : t -> unit
    val path : t -> action:[`Add|`Rem] -> var:[`Build|`Source] -> ?cwd:string -> string -> unit
    val load_packages : t -> string list -> [`Ok | `Failures of (string * exn) list]
    val set_extension : t -> enabled:bool -> string -> unit
  end

  (* Path configuration *)
  val source_path : t -> Path_list.t
  val build_path  : t -> Path_list.t
  val cmt_path    : t -> Path_list.t

  (* List all top modules of current project *)
  val global_modules : t -> string list

  (* Force recomputation of top modules *)
  val flush_global_modules : t -> unit

  (* Enabled extensions *)
  val extensions: t -> Extension.set

  (* Lexer keywords for current config *)
  val keywords: t -> Lexer.keywords
end

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> Parser.state -> t
  val lexer: t -> Lexer.t History.t
  val parser: t -> Parser.t
  val start_lexing: t -> Lexing.position * (Lexing.lexbuf -> Lexer.t History.t)
  val update: t -> Lexer.t History.t -> Parser.t
end

