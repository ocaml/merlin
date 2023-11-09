(** When using Merlin as a library, one should use functions provided by this
      module to customize Merlin's behaviour. *)

(** [set_program] sets the name of the program that will be used in error
    messages. *)
val set_program_name : string -> unit

(** [program ()] returns the name of the program as registered by
    [set_program]. Defaults to "Merlin". *)
val program_name : unit -> string

(** [set_cache_period] sets the file cache retention period. Measured in minutes. *)
val set_cache_period : int -> unit

(** [program ()] returns file cache retention period. Defaults to None. *)
val cache_period : unit -> int option

module Json : sig
    (** Merlin's logger requires a Json pretty-printer for correct operation.
        [set_pretty_to_string] can be used to provide one. A common pretifier
        is [Yojson.Basic.pretty_to_string].  *)
    val set_pretty_to_string : (Std.json -> string) -> unit
end

(** Merlin spawns child processes for preprocessors (pp and ppx), which can be
    customized via [System] *)
module System : sig
  (** [set_run_in_directory] sets an implementation for spawning external
        programs. This is used by Merlin to spawn preprocessors and ppxes. For
        compatibility reasons, there are currently some limitations to how this
        should be implemented:

      - Implementation should expect [prog] to be already quoted and contain
        arguments. This is due to how ppx configuration is passed to Merlin. In
        order to prepare a future transition to more sane argument passing, the
        implementation can look at the [prog_is_quoted] argument to know if it
        is actually safe to quote the command normally (using
        [Filename.quote_command] for example).

      - [prog] might contain shell expansions, command substitutions etc. It
        should therefore be ran under a shell for maximum compatibility. However
        this should never happen when the configuration is generated by Dune.

      - Programs runned by this function should never output on stdout since it
        is the channel used by Merlin to communicate with the editor. One way to
        enforce that is to redirect stdout to stderr.

      - As of today Merlin handles the [`Cancelled] return case identically as
        other error codes. *)
  val set_run_in_directory
    : (prog:string
    -> prog_is_quoted:bool
    -> args:string list
    -> cwd:string
    -> ?stdin:string
    -> ?stdout:string
    -> ?stderr:string
    -> unit
    -> [ `Finished of int | `Cancelled ])
    -> unit
end
