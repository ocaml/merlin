(** Description of a buffer managed by Merlin *)

type buffer = {

  (** Path of the buffer in the editor.
      The path is absolute if it is backed by a file, although it might not yet
      have been saved in the editor.
      The path is relative if it is a temporary buffer. *)
  path : string;

  (** Any flag that has been passed to the reader in .merlin file *)
  flags : string list;

  (** Content of the buffer *)
  text : string;
}

(** ASTs exchanged with Merlin *)
type ast =

  | (** An implementation, usually coming from a .ml file *)
    Structure of Parsetree.structure

  | (** An interface, usually coming from a .mli file *)
    Signature of Parsetree.signature

type outcometree =
  | Out_value          of Outcometree.out_value
  | Out_type           of Outcometree.out_type
  | Out_class_type     of Outcometree.out_class_type
  | Out_module_type    of Outcometree.out_module_type
  | Out_sig_item       of Outcometree.out_sig_item
  | Out_signature      of Outcometree.out_sig_item list
  | Out_type_extension of Outcometree.out_type_extension
  | Out_phrase         of Outcometree.out_phrase

(** Additional information useful for guiding completion *)
type complete_info = {
  (** True if it is appropriate to suggest labels for this completion. *)
  complete_labels : bool;
}

module type V0 = sig
  (** Internal representation of a buffer for the extension.
      Extension should avoid global state, cached information should be stored
      in values of this type. *)
  type t

  (** Turns a merlin-buffer into an internal buffer.

      This function should be total, an exception at this point is a
      fatal-error.

      Simplest implementation is identity, with type t = buffer.
  *)
  val load : buffer -> t

  (** Get the main parsetree from the buffer.
      This should return the AST corresponding to [buffer.source].
  *)
  val parse : t -> ast

  (** Give the opportunity to optimize the parsetree when completing from a
      specific position.

      The simplest implementation is:

          let for_completion t _ = ({complete_labels = true}, (tree t))

      But it might be worthwhile to specialize the parsetree for a better
      completion.
  *)
  val for_completion : t -> Lexing.position -> complete_info * ast

  (** Parse a separate user-input in the context of this buffer.
      Used when the user manually enters an expression and ask for its type or location.
  *)
  val parse_line : t -> Lexing.position -> string -> ast

  (** Given a buffer and a position, return the components of the identifier
      (actually the qualified path) under the cursor.

      This should return the raw identifier names -- operators should not be
      surrounded by parentheses.

      An empty list is a valid result if no identifiers are under the cursor.
  *)
  val ident_at : t -> Lexing.position -> string Location.loc list

  (** Opposite direction: pretty-print a tree.
      This is used for displaying answers to queries.
      (type errors, signatures of modules in environment, completion candidates, etc).
  *)
  val print_outcome : Format.formatter -> outcometree -> unit
end
