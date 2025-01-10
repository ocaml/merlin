module type P = sig val txt : string end

module Make (Params : sig val txt : string end) = struct
  include Params
end
