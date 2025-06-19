val substitute :
  start:Lexing.position ->
  stop:Lexing.position ->
  ?extract_name:string ->
  Mconfig.t ->
  Msource.t ->
  Mtyper.typedtree ->
  Query_protocol.substitution_result option
