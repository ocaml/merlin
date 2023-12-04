module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end
module type Comparable = sig
  type t
  val compare : t -> t -> int
end
module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end

module type S = sig
  type t
  module Sub : sig
    type outer := t
    type t
    val to_outer : t -> outer
  end
end

module type ENDO = sig
  module type T
  module F: T -> T
end
module Endo(X: sig module type T end): ENDO with module type T = X.T =
struct
    module type T = X.T
    module F(X:T) = X
end;;