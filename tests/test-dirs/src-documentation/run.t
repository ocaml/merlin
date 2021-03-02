documentation for the last defined value (in the same file) is shown
  $ $MERLIN single document -position 7:10 <<EOF
  > (** first function *)
  > let f () = ()
  > 
  > (** second function *)
  > let g () = ()
  > 
  > let () = g (f ())
  > 
  > let list_rev = List.rev
  > EOF
  {
    "class": "return",
    "value": "second function",
    "notifications": []
  }

documentation for the non-last defined value (in the same file) is show
(we care about "non-last" value because of issue #1261)
  $ $MERLIN single document -position 7:13 <<EOF
  > (** first function *)
  > let f () = ()
  > 
  > (** second function *)
  > let g () = ()
  > 
  > let () = g (f ())
  > 
  > let list_rev = List.rev
  > EOF
  {
    "class": "return",
    "value": "first function",
    "notifications": []
  }


