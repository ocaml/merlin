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

(* *** Don't select deprecated paths *** *)

include struct
  [@@@warning "-3"]

  module M = struct
    type t = T
    [@@deprecated "bad"]
  end

  type t = M.t
  [@@deprecated "bad"]

  module N = struct
    module O = struct
      type t = M.t
    end
  end

  let f (x : t) : unit = x

end
