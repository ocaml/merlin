val locs_of
  : config:Mconfig.t
  -> env:Env.t
  -> local_defs:Mtyper.typedtree
  -> pos:Lexing.position
  -> string
  -> (Warnings.loc list, string) result
