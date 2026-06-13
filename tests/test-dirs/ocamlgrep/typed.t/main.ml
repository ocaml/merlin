(* Mix of float-returning and int-returning expressions. The typed
   query [(__ : float)] should only catch the float ones. *)
let f () = 1.0 +. 2.0
let g () = 1 + 2
let h () = sqrt 9.0
let _used = (f (), g (), h ())
