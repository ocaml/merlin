open Misc

(* Project configuration *)
module Project : sig
  (* Current buffer path *)
  val set_local_path : string -> unit

  (* Project-wide configuration *)
  val set_dot_merlin : Dot_merlin.path_config -> unit

  val reset_project : unit -> unit

  (* Config override by user *)
  val reset_user : unit -> unit
  val user_path : action:[`Add | `Rem] ->
                  var:[`Build | `Source] ->
                  ?cwd:string -> string -> unit

  val user_load_packages : string list -> unit
  val user_set_extension : enabled:bool -> string -> unit

  (* Output values *)
  val source_path : Path_list.t
  val build_path  : Path_list.t
  val cmt_path    : Path_list.t

  (* List all top modules of current project *)
  val global_modules : unit -> string list
  (* Force recomputing list of global modules *)
  val flush_global_modules : unit -> unit
end
