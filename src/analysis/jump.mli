val get :
  (Merlin_typer.content * 'a) list ->
  Std.Lexing.position ->
  string -> [> `Error of string | `Found of Lexing.position ]
