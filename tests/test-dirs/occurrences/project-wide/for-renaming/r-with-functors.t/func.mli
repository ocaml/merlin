module type P = sig val txt : string end

module Make (_ : P) : sig
  include P
end
