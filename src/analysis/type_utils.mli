open Std

val verbosity : int Fluid.t

module Printtyp : sig
  include module type of struct include Printtyp end

  val type_declaration :
    Env.t -> Ident.t -> Format.formatter -> Types.type_declaration -> unit

  val type_scheme : Env.t -> Format.formatter -> Types.type_expr -> unit

  val modtype : Env.t -> Format.formatter -> Types.module_type -> unit

  val wrap_printing_env : Env.t -> verbosity:int -> (unit -> 'a) -> 'a
end

val mod_smallerthan : int -> Types.module_type -> int option
(** Check if module is smaller (= has less definition, counting nested ones)
    than a particular threshold. Return (Some n) if module has size n, or None
    otherwise (module is bigger than threshold).
    Used to skip printing big modules in completion. *)

val type_in_env : ?verbosity:int -> ?keywords:Raw_lexer.keywords ->
  Env.t -> Std.Format.formatter -> string -> bool
(** [type_in_env env ppf input] parses [input] and prints its type on [ppf].
    Returning true if it printed a type, false otherwise. *)
