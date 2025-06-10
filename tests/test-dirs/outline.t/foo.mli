module Bar : sig
  type t = int

  module type S1 = sig
    type t
    val foo : t -> int
  end
end

class type class_type_a = object
  method a : int -> int
end

class class_b : object
  method b : string -> string
end

exception Ex of char

type ('a, 'b) eithery = Lefty of 'a | Righty of 'b

type 'a point = { x : 'a; y : 'a; z : 'a }

class a : object end

and b : object end

and c : object end

class type ta = object end

and tb = object end

class b : object
  val foo : int
  method bar : unit -> unit
end

and c : object end

class a : object
  val b : < inside_a_b : unit -> unit >
end

and b : object
  val foo : int
  method bar : unit
end

class type ta = object
  method baz : int -> int -> string
end

and tb = object end

val final_let : < foo : int >
