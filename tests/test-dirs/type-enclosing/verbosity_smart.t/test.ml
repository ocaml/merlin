module type T = sig 
  val u : unit
end

module M = struct
  let u = ()
end

module F () : T = struct 
  let u = ()
end

module MA = M

module FT = F ()
