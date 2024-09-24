open Misc

module Path : sig
  (** [to_shortest_lid ~env ~env_check path] will make a [Longident.t] from the
  provided [Path.t] and attempt to use the shortest prefix possible given the
  currently opened modules. The result is checked by looking it up in the
  environment using the [env_check : Longident.t -> Env.t -> 'a] function.

  The check is needed because shadowing can cause subtle issues. A typical check
  function would be [Env.find_constructor_by_name]. WHen the check fails the
  function will return [Untypeast.lident_of_path path] instead of clever
  prefix-less constructions.

  Optionally a [name] can be provided that will be used as the last ident of the
  path. *)
  val to_shortest_lid :
    env:Env.t ->
    ?name:string ->
    env_check:(Longident.t -> Env.t -> 'a) ->
    Path.t ->
    Longident.t

  (* Return whether the given path is opened in the given environment *)
  val is_opened : Env.t -> Path.t -> bool
end

(* Add parenthesis to qualified operators *)
val parenthesize_name : string -> string

(** [parse_identifier] attempts to re-parse a longident so that we get
    the location of each of its components. *)
val parse_identifier :
  Mconfig.t * Msource.t -> Lexing.position -> modname Location.loc list
