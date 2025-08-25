module Compl_app_ctxt = struct
  let foo ~i ~j = i + j
  let bar = 10
  let y = foo ~i:5
end

module Compl_disambiguation = struct
  module Constr = struct
    module T = struct
      type t = Foobar
    end
    let _foobar = (Foo : T.t)
  end

  module Record = struct
    module T = struct
      type t = { foobar : int; test_other : float }
    end

    let _easy = { T.f }

    let _hard = ({ foo } : T.t)

    open T

    let _easier = { foobar = 5; tes }
  end
end

module Compl_expansion = struct
  module Expansion1 = struct
    let x = L.m
  end
  module Expansion2 = struct
    let x = Lsi.m
  end
end

module Compl_infix = struct
  module Z = struct
    let ( >> ) = 0
    let ( |+ ) = 0
    let ( |- ) = 0
    let ( >>= ) = 0
    let ( >>| ) = 0
  end

  let _ = Z.
end

module Compl_kind = struct
  let funnyny = fun ny -> ny

  let _ = fu

  let _ = List.f
end

module Compl_parent = struct
  module MyList = struct
    [@@@ocaml.warning "-65"]
    type 'a t =
      | (::) of 'a * 'a t
      | []
    type u = ()
    let (mod) = ()
    let random = 1
  end

  let _ = MyList.

end

module Compl_1575_1 = struct
  let goo = object
    method bar i = i + 5
    method bazs _k = "hello"
  end

  let something = go
end

module Compl_1575_2 = struct
  let foo = object
    method bar i = i + 5
    method bazs _k = "hello"
  end

  let something = foo#
end

module Compl_1575_3 = struct
  let foo = object
    method bar i = i + 5
    method bazs _k = "hello"
  end

  let something = foo#baz
end

module Compl_1575_4 = struct
  module A = struct
    let foo = object
      method bar i = i + 5
      method bazs _k = "hello"
    end
  end

  let something = A.foo#ba
end

module Compl_lsp_503 = struct
  [1;2]|>List.ma
end

module Construct_c_modules = struct
  module Functor_app = struct
    module type X_int = sig val x : int end

    module Increment (M : X_int) = struct
      let x = M.x + 1
    end

    module X = Increment(_);;
  end

  module Module = struct
    module type S = sig
      type t = private b and b = A | B of t
      type (-'a, +'b) t' = T of ('a -> 'b)
      type t2 = A | B of string | C of t
      type nonrec r = { lbl1 : t; lbl2 : float list}
      type nonrec n = r and m = float
      type t_ext = ..
      type t_ext += Str of string | A
      type v = [`A of t_ext]

      val i : int
      val f : t -> int

      module Sub : sig
        val y : int
      end

      class type room = object
        val mutable gene : unit

        method scientific : unit -> int
      end
      class croom : room
      module type Another = sig val i : int end

      module type Sig = sig
        type t and b
        val f : int -> float
        module type STyp = sig end
        module D : Another
      end

      module Submod : Sig

      module SubFunc (M : Sig) : sig val g : unit end
    end

    module type Small = sig type t = int end

    module M : S = _

    let m : (module Small) = _

    let m = (module _ : Small)
  end
end

module Construct_c_depth = struct
  module D1 = struct
    let x : int option option = _
  end
  module D2 = struct
    type t = { a : int option option; b : float option }
    let x : t = _
  end
  module D3 = struct
    type t = int option option * float option
    let x : t = _        
  end
end

module Construct_c_errors_a = struct
  module E1 = struct end
  module E2 =
    let x : int = 
end
end

module Construct_c_errors_b = struct
  module E3 = struct
    let _ = 3
  end
  module E4 = struct
    module M = N module type S = module type of M
    module M : S = _
  end
  module E5 = struct
    module M : S = _
  end
  module E6 = struct
    module M = _
  end
end

module Construct_c_fun = struct
  module Fun1 = struct
    module Mymod = struct type the_type = int end
    type the_type = float
    let x : Mymod.the_type -> the_type -> unit =
      _
  end
  module Fun2 = struct
    module Mymod = struct type int = string end
    type int = float
    let x : Mymod.int -> int -> unit =
      _
  end
  module Fun3 = struct
    module Mymod :
      sig type t val x : t val f : int -> t end =
      struct type t = int let x = 3 let f x = 2 * x end
    type t = float
    let g x = 2 *. x
    let x : Mymod.t = 3
    let z : Mymod.t =
      _
    let t : t =
      _
  end
end

module Construct_inline_record_issue1617 = struct
  type foo_record = { foo : int }

  type foo = Foo of foo_record
  type bar = Bar of { bar : int }

  let x : foo = _ (* succeeds *)
  let y : bar = _ (* fails *)
end

module Construct_obj = struct
  module Obj1 = struct
    let o : < a : string; get : int -> int option >
      = _
  end
  module Obj2 = struct
    let a : < x : int >
      = _
    let b : < x : int; .. >
      = _
    type o = < x : int >
    let x : < y: char; o >
      = _
  end
end

module Construct_sum_par = struct
  let x : int option option = Some (_)
end

module Construct_prefix = struct
  module C1 = struct
    module Prefix = struct
      type t = A of int | B
    end
    let x : Prefix.t = _
  end
  module C2 = struct
    module Prefix = struct
      type t = A of int | B
    end
    open Prefix
    let x : t = _
  end
  module C3 = struct
    module Prefix = struct
      type t = A of int | B
      type r = { a : t }
    end
    let x : Prefix.t = _
    let x : Prefix.r = _
    open Prefix
    let x : t = _
    let x : r = _
  end
end

module Construct_simple = struct
  module C1 = struct
    let nice_candidate = Some 3
    let nice_candidate_with_arg x = Some x
    let nice_candidate_with_labeled_arg ~x = Some x
    let y = 4
    let x : int option = _
  end
  module C2 = struct
    let x : int list = _
  end
  module C3 = struct
    let x : 'a list = _
  end
  module C4 = struct
    let x : int lazy = _
  end
  module C5 = struct
    type my_type = One | Another
    let x : my_type = _
  end
  module C6 = struct
    type r = { a : string; b : int option }
    let nice_candidate = {a = "a"; b = None }
    let x : r = _
  end
  module C7 = struct
    let nice_candidate s = int_of_string s
    let x : string -> int = _
  end
  module C8 = struct
    let type mytype = float
    let x : v:string -> float -> mytype -> mytype -> int = _
  end
  module C9 = struct
    type tup = int * float * (string option)
    let some_float = 4.2
    let x : tup = _
  end
  module C10 = struct
    type v = [ `A | `B of string ]
    let some_v = `B "totoro"
    let x : v = _
  end
  module C11 = struct
    type _ term =
      | Int : int -> int term
      | Float : float -> float term
      | Eq : 'a term * 'a term -> 'a term

    let x : 'a term =
      _

    let x : int term =
      _
  end
  module C12 = struct
    type _ term =
      | Int : int -> int term
      | Float : float -> float term
      | Add : (int -> int -> int) term
      | App : ('b -> 'a) term * 'b term -> 'a term
    let v1 = Int 42
    let v2 = Float 3.5
    let x : 'a term =
      _
    let x : int term =
      _
  end
  module C13 = struct
    type 'a t = A of 'a
    let x = A _
  end
  module C14 = struct
    let x : type a . a list = _
  end
  module C15 = struct
    let x : int =         _
    let x : nativeint =   _
    let x : int32 =       _
    let x : int64 =       _
    let x : float =       _
    let x : char =        _
    let x : string =      _
    let x : bool =        _
    let x : unit =        _
    let x : exn =         _
    let x : 'a array =    _
    let x : 'a lazy_t =   _
  end
end

(* REMOVED BECAUSE IT LEADS TO TOO MUCH DISTRACTION *)
(* module Construct_hole = struct *)
(*   module H1 = struct end *)
(*   module H2 = struct *)
(*     let x : int option = _ *)
(*     let g x y = x * y *)
(*     let f x y = g _ _ *)
(*     module M : sig val f : int -> unit end = _ *)
(*   end *)
(* end *)

module Deprecation_a = struct
  module Foo : sig
    
    val bar : unit -> int
    [@@ocaml.deprecated "deprecation message"]
      
    val baz : unit -> unit
  end = struct
    let bar () = 42
    let baz () = 48
  end

  let x = Foo.ba
end

module Destruct_a = struct
  module A = struct
    type void = |
    let f (x : void option) =
      match x with
      | None -> ()
  end
  module B = struct
    let _ =
      match (None : int option option) with
      | Some (Some 3) -> ()
  end
  module C = struct
    type funny = int option -> unit
    let v : funny = function
      | None -> ()
  end
  module D = struct
    type a = A | B of string
    type recd = { a : a }
    let f (x : recd) =
      match x with
      | { a = A } -> ()
  end
  module E = struct
    type a = A | B of string
    let f (x : recd) =
      match x with
      | { a = A } -> ()
      | { a = B _ } -> ()
  end
  module F = struct
    type basic_color = [ `Blue | `Red | `Yellow ]
    let f (x : basic_color) =
      match x with
      | `Blue -> ()
  end
  module G = struct
    type basic_color = [ `Blue | `Red | `Yellow ]
    type better_color = [ basic_color | `Gold ]
    let f (x : better_color) =
      match x with
      | #basic_color -> ()                 
  end
  module H = struct
    type _ term =
      | Int : int -> int term
      | Add : (int -> int -> int) term
      | App : ('b -> 'a) term * 'b term -> 'a term

    let eval : type a. a term -> unit =
      fun (x : a term) -> match x with
        | Int _ -> ()
        | Add -> ()
  end
  module I = struct
    type _ term =
      | Int : int -> int term
      | Add : (int -> int -> int) term
      | App : ('b -> 'a) term * 'b term -> 'a term
    let eval (type a) : a term -> unit =
      function
      | Int _ -> ()
      | Add -> ()
  end
  module J = struct
    type _ t =
      | I : int t
      | B : bool t
    let f : int t -> unit =
      function
      | I -> ()
  end
  module K = struct
    module T = struct type t = A | B of int end
    let g x =
      match x with
      | T.A -> ()
  end
  module L = struct
    module T = struct type t = A | B of int end
    let g = function
      | T.A -> ()
  end
end
