val from_string : sources:string list -> env:Env.t -> local_modules:(string * Location.t) list ->
  string -> (string * Location.t) option
