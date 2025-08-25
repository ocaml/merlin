module Foo = struct
  type t = { foo : int; bar : int }

  let foo = "hello"
end

let _ =
  let foo = 10 in
  let bar = 10 in
  ({ Foo.foo; bar } : Foo.t)
