val get :
  (_ * Merlin_typer.typed * _) list ->
  Std.Lexing.position ->
  string -> [> `Error of string | `Found of Lexing.position ]
