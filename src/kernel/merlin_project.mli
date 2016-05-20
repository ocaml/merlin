type t

(* Copy global state after initialization *)
val initialized : unit -> unit

(* A global store mapping (.merlin-)path to projects *)
val get : string list -> t

(* Current buffer path *)
val set_local_path : t -> string list -> unit

(* Project-wide configuration *)
val check_dot_merlin : t -> unit
val get_dot_merlins_failure : t -> (string * exn) list

(* paths of dot_merlins *)
val get_dot_merlins : t -> string list

(* Dump all the flags given to merlin. *)
val get_flags : t -> (string * string list list) list

(* Path configuration *)
val source_path : t -> string list
val build_path  : t -> string list
val cmt_path    : t -> string list

(* List all top modules of current project *)
val global_modules : t -> string list

(* Reader *)
val reader: t -> string list

(* Enabled extensions *)
val extensions: t -> Extension.set

(* Suffixes to search for located files with. *)
val suffixes:  t -> (string * string) list

(* Lexer keywords for current config *)
val keywords: t -> Merlin_lexer.keywords

(* Make global state point to current project *)
val setup : t -> string list -> unit

(* User config override *)
val get_user_config : t -> Dot_merlin.config
val set_user_config : t -> Dot_merlin.config -> unit
val get_user_config_failures : t -> (string * exn) list

(* Invalidate cache *)
val version_stamp: t -> int ref
