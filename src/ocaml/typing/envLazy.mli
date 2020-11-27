open! Result_compat (* merlin *)

type ('a,'b) t

type log

val force : ('a -> 'b) -> ('a,'b) t -> 'b
val create : 'a -> ('a,'b) t
val get_arg : ('a,'b) t -> 'a option
val create_forced : 'b -> ('a, 'b) t
val create_failed : exn -> ('a, 'b) t

(* [force_logged log f t] is equivalent to [force f t] but if [f] returns
   [None] then [t] is recorded in [log]. [backtrack log] will then reset all
   the recorded [t]s back to their original state. *)
val log : unit -> log
val force_logged :
  log -> ('a -> ('b, 'c) result) -> ('a,('b, 'c) result) t -> ('b, 'c) result
val backtrack : log -> unit

(* For compatibility with 4.02 and 4.03 *)
val is_val : ('a, 'b) t -> bool
type ('a, 'b) eval =
  | Done of 'b
  | Raise of exn
  | Thunk of 'a
val view : ('a, 'b) t -> ('a, 'b) eval

(* For compatibility with 4.08 and 4.09 *)
val force_logged_408 :
  log -> ('a -> 'b option) -> ('a,'b option) t -> 'b option
