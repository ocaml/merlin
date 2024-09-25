val get_rewrites :
  mode:[> `Qualify | `Unqualify ] ->
  Mtyper.result ->
  Lexing.position ->
  (string * Location.t) list
