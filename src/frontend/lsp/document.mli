type t

val make :
  ?version:int
  -> uri:Lsp.Protocol.documentUri
  -> text:string
  -> unit
  -> t

val uri : t -> Lsp.Protocol.documentUri
val source : t -> Msource.t
val with_pipeline : t -> (Mpipeline.t -> 'a) -> 'a
val version : t -> int

val update_text : ?version:int -> Lsp.Protocol.DidChange.textDocumentContentChangeEvent -> t -> t
