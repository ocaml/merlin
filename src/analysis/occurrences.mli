val locs_of
  : config:Mconfig.t
  -> source:Msource.t
  -> env:Env.t
  -> typer_result:Mtyper.result
  -> pos:Lexing.position
  -> string
  -> (Warnings.loc list, string) result
