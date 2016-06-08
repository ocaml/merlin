(* Merlin should detect two errors :
   - ("hello" ^ 2) be a string
   - 2 should be a string

   If the typechecker doesn't recover properly, one error might be missed.

   Then, type enclosing should give two types for 2: int and string.
   Otherwise it means that either
   - Browse cannot traverse broken nodes in Typedtree,
   - nodes where dropped during typechecking.
 *)
let x = ("hello" ^ 2) * 5
