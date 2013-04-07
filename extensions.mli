(* Adjust typing environment for syntax extensions.
 * See [Fake] for AST part *)

(* Extension environment is composed of two part:
 * - private definitions, not exposed to user, but accessed from,
 * - public definitions, those are made available to user like Pervasive
 *   module.
 * See [Typer.initial_env] for initial environment generation.
 *)

type extension = Parsetree.signature * Parsetree.signature

(* Private definitions are put in a fake module named "_" with the following
 * ident. Use it to test or find private definition. *)
val ident : Ident.t

(* Known extensions *)
val registry : extension list

(* Register extensions in environment.
 * If an extension fails to typecheck (e.g. it needs definitions from an
 * external package not loaded), it is ignored and registration
 * continue for other extensions. *)
val register : Env.t -> Env.t
