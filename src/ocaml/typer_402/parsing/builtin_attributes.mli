val string_of_payload: Parsetree.payload -> string option
val warning_enter_scope: unit -> unit
val warning_leave_scope: unit -> unit
val warning_attribute: Parsetree.attributes -> unit
val with_warning_attribute: Parsetree.attributes -> (unit -> 'a) -> 'a
