val from_string
  : project:Merlin_lib.Project.t
  -> env:Env.t
  -> local_defs:Typedtree.structure list
  -> string
  -> (string option * Location.t) option
