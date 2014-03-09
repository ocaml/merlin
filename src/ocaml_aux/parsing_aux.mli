open Std
exception Warning of Location.t * string

val warnings: exn list ref option fluid
val raise_warning: exn -> unit
val prerr_warning: Location.t -> Warnings.t -> unit
val catch_warnings: exn list ref -> (unit -> 'a) -> 'a or_exn

val location_union : Location.t -> Location.t -> Location.t
val compare_pos: Lexing.position -> Location.t -> int

val with_bag_of_holding: Location.t -> exn -> Location.t
val bag_of_holding: Location.t -> exn

val pack_fake_start: Location.t -> Lexing.position -> Location.t
val unpack_fake_start: Location.t -> Lexing.position

val reconstruct_struct: (exn -> Parsetree.structure) ref
