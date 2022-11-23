(** When using Merlin as a library, one should use functions provided by this
      module to customize Merlin's behaviour. *)

(** [set_program] sets the name of the program that will be used in error
    messages. *)
val set_program_name : string -> unit

(** [program ()] returns the name of the program as registered by
    [set_program]. Defaults to "Merlin". *)
val program_name : unit -> string

module Json : sig
    (** Merlin's logger requires a Json pretty-printer for correct operation.
        [set_pretty_to_string] can be used to provide one. A common pretifier
        is [Yojson.Basic.pretty_to_string].  *)
    val set_pretty_to_string : (Std.json -> string) -> unit
end
