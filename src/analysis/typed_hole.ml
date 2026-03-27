let syntax_repr = "_"
let can_be_hole s = String.equal syntax_repr s

(* the pattern matching below is taken and modified (minimally, to adapt the
   return type) from [Query_commands.dispatch]'s [Construct] branch;

   If we directly dispatched [Construct] command to merlin, we'd be doing
   useless computations: we need info whether the expression at the cursor is a
   hole, we don't need constructed expressions yet.

   Ideally, merlin should return a callback [option], which is [Some] when the
   context is applicable. *)
let is_a_hole = function
  | (_, Browse_raw.Module_expr { mod_desc = Tmod_typed_hole; _ }) :: (_, _) :: _
  | (_, Browse_raw.Expression { exp_desc = Texp_typed_hole; _ }) :: _ -> true
  | [] | (_, _) :: _ -> false
