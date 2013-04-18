(** {0 Outline parser}
 * Auxiliary definitions used by the outline parser *)

type offset = History.offset
type position = Lexing.position

(** Source code constructs are split into "chunks" of different "kinds". *)
type kind =
  | Enter_module (* module _ = struct *)
  | Leave_module (* end *)
  | Definition   (* let / type / … *)
  | Rollback     (* and … : we should go back to the previous definition
                  * to find its "kind" *)
  | Done         (* EOF found after a syntactically correct construction *)
  | Unterminated (* Unfinished syntactic construction *)
  | Syntax_error of Location.t

val kind_to_string : kind -> string

(** The outline parser proceeds by side-effects:
  * - most productions have no attached semantic action
  * - when the parser finds a syntactic construct that it knows how to chunk
  *   (above: module or definition), it raises the [Chunk] exception.
  * The associated position is the position of the last token of the
  * chunk, so that, if the parser consumed a lookahead token, it can
  * be recognized and added back to the lexing buffer.
  * EX: let a = 5 type t = ...
  *                  ^ |CHUNK: let a = 5|
  *   The parser raises [Chunk] only after [type], so we must add the
  *   [type] token back to the input stream.
  *)
exception Chunk of kind * position

(** Called to (re)initialize the parser. (nesting := 0) *)
val reset : unit -> unit

(** Used to ignore first-class modules.
  * The construct "let module = … in " allows to define a module
  * locally inside a definition, but our outline parser cannot work
  * inside a definition (it is either correct as a whole,
  * or incorrect).
  * After call to [enter_sub], such constructions are not reported
  * until a call to its dual [leave_sub] is made.
  *)
val enter_sub : unit -> unit
(** Decrements [nesting] *)
val leave_sub : unit -> unit
(** Sends [Chunk] only when outside of any enter_sub/leave_sub *)
val emit_top : kind -> position -> unit
