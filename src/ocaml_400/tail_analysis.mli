open Parsetree

(* Assuming an expression (or other constructs occuring inside expressions,
   like cases of a match statement) is in tail-position, returns all
   sub-expression that will be evaluated in tail-position too *)
val tail_positions: BrowseT.node -> BrowseT.node list

(* If the node is a function, return all of its entry-points -- those are in
   tail-position. Returns an empty list otherwise *)
val entry_points: BrowseT.node -> BrowseT.node list

val is_call: BrowseT.node -> bool
