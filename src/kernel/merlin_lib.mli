open Misc

(* Stateful parts:
   - typer snapshot root, done Buffer.btype
   - env cache, done Buffer.env
   - lexer keywords, done Lexer
   - compiler path, done Project
   - compiler flags, todo
*)

module Lexer: sig
  type keywords = Raw_lexer.keywords

  (* Lexing is split in two steps.

     First the list of tokens is represented by a [item History.t].
     It's a pure value, independent of the context.

     Second the process of lexing is represented by values of type [t].  You
     resume the process from an arbitrary list of tokens, feeding it with one
     or more string, and you can extract the current list of tokens and cursor
     position at any time.
     Beware, the cursor may be in the middle of a not yet determined token.

     The process ultimately ends when fed with the empty string, representing
     EOF.
  *)

  (* Lexing step *)
  type item =
    | Valid of Lexing.position * Raw_parser.token * Lexing.position
    | Error of Raw_lexer.error * Location.t

  val item_start: item -> Lexing.position
  val item_end: item -> Lexing.position

  (** Create an empty list new lexer *)
  val empty: filename:string -> item History.t

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  type t
  val history: t -> item History.t
  val start: keywords -> item History.t -> t
  val position: t -> Lexing.position
  val feed: t -> string -> bool
  val eof: t -> bool
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

  val lexer: t -> Lexer.item History.t
  val update: t -> Lexer.item History.t -> unit
  val start_lexing: t -> Lexer.t

  val parser: t -> Parser.t
  val path: t -> Parser.path
  val typer: t -> Env.t * Typedtree.structure list
end

