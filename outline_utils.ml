type chunk =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done

exception Chunk of chunk * Lexing.position

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
