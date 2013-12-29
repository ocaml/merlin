open Misc

(* Stateful parts:
   - typer snapshot root, done Buffer.btype
   - env cache, done Buffer.env
   - lexer keywords, done Lexer
   - compiler path, done Project
   - compiler flags, todo
*)

module Lexer: sig
  type item =
    | Valid of Raw_parser.token
    | Error of Raw_lexer.error

  (* Location of the last valid item *)
  type t = item Location.loc

  (** Create a new lexer *)
  val empty: filename:string -> t History.t

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  val start: Raw_lexer.keywords -> t History.t -> Lexing.position * (Lexing.lexbuf -> t History.t)
end

module Parser : sig
  type t
  type frame

  type state = Raw_parser.state

  val implementation : state
  val interface : state

  val from : state -> t
  val feed : Lexing.position * Raw_parser.token * Lexing.position
           -> t -> [`Accept of Raw_parser.semantic_value | `Reject of Raw_parser.step Raw_parser.parser | `Step of t]

  val stack : t -> frame option
  val depth : frame -> int

  val value : frame -> Raw_parser.semantic_value
  val eq    : frame -> frame -> bool
  val next  : frame -> frame option

  val of_step : ?hint:MenhirUtils.witness -> Raw_parser.step Raw_parser.parser
              -> [`Accept of Raw_parser.semantic_value | `Reject of Raw_parser.step Raw_parser.parser | `Step of t]
  val to_step : t -> Raw_parser.feed Raw_parser.parser option
end

(* Project configuration *)
module Project : sig
  type t

  (** Create a new project *)
  val create: unit -> t

  (* Current buffer path *)
  val set_local_path : t -> string -> unit

  (* Project-wide configuration *)
  val set_dot_merlin : t -> Dot_merlin.config option -> [`Ok | `Failures of (string * exn) list]

  (* Config override by user *)
  module User : sig
    val reset : t -> unit
    val path : action:[`Add|`Rem] -> var:[`Build|`Source] -> ?cwd:string -> string -> unit
    val load_packages : string list -> [`Ok | `Failures of (string * exn) list]
    val set_extension : enabled:bool -> string -> unit
  end

  (* Path configuration *)
  val source_path : t -> Path_list.t
  val build_path  : t -> Path_list.t
  val cmt_path    : t -> Path_list.t

  (* List all top modules of current project *)
  val global_modules : t -> string list

  (* Force recomputation of top modules *)
  val flush_global_modules : t -> unit
end

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> t
  val lexer: t -> Lexer.t History.t
  val start_lexing : t -> (Lexing.lexbuf -> Lexer.t History.t)
  val update_lexer : t -> Lexer.t History.t -> unit
end

