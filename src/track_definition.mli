val from_string
  : sources:string list
  -> env:Env.t
  -> local_defs:Typedtree.structure Asttypes.loc list
  -> local_modules:(string * Location.t) list
  -> string
  -> (string option * Location.t) option
