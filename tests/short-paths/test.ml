(* *** #670 *** *)

module type S = sig
  class virtual x : object
    method private virtual release : unit
  end
end

module Make (C : S) = struct

  class c =
    object
      method private release = ()
    end

  let x = 3
end

(* *** #843 *** *)

class a x = object method x = x end and b x = a x

class a x = object end and b = object inherit a end

(* *** #907 *** *)

class test ?a =
object
  method b = c
end

(* the following cases do not trigger error *)

class test a =
object
  method b = c
end

class test ?a =
object
  method b = ()
end
