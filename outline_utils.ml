type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated

exception Chunk of kind * Lexing.position

let nesting = ref 0

let reset () =
  nesting := 0

let enter () =
  incr nesting

let leave () =
  (if !nesting <= 0
   then failwith "Outline_utils.leave: invalid nesting");
  decr nesting

let emit_top c pos =
  prerr_endline "emit";
  if !nesting = 0 then raise (Chunk (c,pos))
