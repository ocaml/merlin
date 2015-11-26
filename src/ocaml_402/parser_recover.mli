open Parser_raw

val default_value : 'a MenhirInterpreter.symbol -> 'a

type action =
  | Shift  : 'a MenhirInterpreter.symbol -> action
  | Reduce : int -> action
  | Sub    : action list -> action
  | Pop    : action

type decision =
  | Action : action -> decision
  | Parent : (int -> action) -> decision

val decision : int -> decision
