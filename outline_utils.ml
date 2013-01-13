type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated
  | Exception of exn

exception Chunk of kind * Lexing.position

let filter_first = ref 0
let nesting = ref 0

let reset ~rollback () =
  filter_first := rollback;
  Printf.eprintf "rollback = %d\n%!" rollback;
  nesting := 0

let enter () =
  incr nesting

let leave () =
  (if !nesting <= 0
   then failwith "Outline_utils.leave: invalid nesting");
  decr nesting

let emit_top c pos =
  prerr_endline "emit";
  if !nesting = 0 then
    begin
      if !filter_first < 0
      then failwith "Outline_utils.emit_top: invalid filter_first"
      else if !filter_first = 0
      then raise (Chunk (c,pos))
      else decr filter_first
    end

let pos_to_json pos =
  Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                  "col", `Int (pos.pos_cnum - pos.pos_bol);
                  "offset", `Int pos.pos_cnum])
