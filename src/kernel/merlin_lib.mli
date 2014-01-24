open Misc

(* Stateful parts:
   - lexer keywords, done Lexer
   - typer snapshot & env, done Typer
   - compiler path, done Project
   - compiler flags, done Project
*)

module Lexer : module type of Merlin_lexer
with type item = Merlin_lexer.item
 and type t = Merlin_lexer.t

module Parser : module type of Merlin_parser
  with type t = Merlin_parser.t
   and type frame = Merlin_parser.frame
   and module Integrate = Merlin_parser.Integrate
   and module Path = Merlin_parser.Path

module Typer : module type of Merlin_typer
  with type t = Merlin_typer.t

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

  (* Invalidate cache *)
  val invalidate: ?flush:bool -> t -> unit
end

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> Parser.state -> t

  val lexer: t -> Lexer.item History.t
  val update: t -> Lexer.item History.t -> unit
  val start_lexing: t -> Lexer.t

  val path: t -> Parser.path
  val parser: t -> Parser.t
  val parser_errors: t -> exn list
  val typer: t -> Typer.t
end

