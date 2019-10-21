
type t
val make : unit -> t
val put : t -> Document.t -> unit
val get : t -> Lsp.Protocol.documentUri -> (Document.t, string) result
val get_opt : t -> Lsp.Protocol.documentUri -> Document.t option
