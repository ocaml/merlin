open Parser_raw

val print_symbol : MenhirInterpreter.xsymbol -> string
val print_value : 'a MenhirInterpreter.symbol -> 'a -> string
val print_token : token -> string
