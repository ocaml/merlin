let x = 3

type x = Foo

let foo : x = Foo

type 'a t = 'a

type 'a s = 'a t

type 'a l = 'a list

module M = struct
  type 'a t = 'a
end

type 'a v = Foo of 'a