open Parser_raw

val default_value : 'a MenhirInterpreter.symbol -> 'a

type decision =
  | Shift  : 'a MenhirInterpreter.symbol * 'a -> decision
  | Reduce : int -> decision
  | Parent : (int -> decision) -> decision
  | Pop    : decision

val decision : int -> decision
