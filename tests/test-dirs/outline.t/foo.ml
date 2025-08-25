module Bar = struct
  type t = int
  module type S1 = sig
    type t

    val foo : t -> int
  end

  class type b = object end
end

class type class_type_a = object
  method a : int -> int
end

class class_b =
  object
    method b s = s ^ s
  end

exception Ex of char

type ('a, 'b) eithery = Lefty of 'a | Righty of 'b

type 'a point = { x : 'a; y : 'a; z : 'a }

class a = object end

and b = object end

and c = object end

class type ta = object end

and tb = object end

class b =
  object
    val foo = 10
    method bar () = print_endline "bar"
  end

and c = object end

class a =
  object
    val b =
      object
        method inside_a_b () =
          let x_inside_a_b = 10 in
          print_int x_inside_a_b
      end
  end

and b =
  object
    val foo = 10
    method bar = print_endline "bar"
  end

class type ta = object
  method baz : int -> int -> string
end

and tb = object end

let final_let =
  let c =
    object
      method foo = 10
    end
  in
  c
