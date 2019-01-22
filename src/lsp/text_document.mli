open Protocol

type t

val make : ?version:int -> documentUri -> string -> t

val documentUri : t -> documentUri
val version : t -> int
val text : t -> string

val apply_content_change : ?version:int -> DidChange.textDocumentContentChangeEvent -> t -> t
