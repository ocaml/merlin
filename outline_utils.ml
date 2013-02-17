type offset = History.offset
type position = Lexing.position

type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated
  | Syntax_error of Location.t
  | Exception of exn

exception Chunk of kind * position

let filter_first = ref 0

(** Used to ignore first-class modules.
  * The construct "let module = … in " allows to define a module
  * locally inside a definition, but our outline parser cannot work
  * inside a definition (it is either correct as a whole,
  * or incorrect).
  * [nesting] is incremented at the beginning of such constructions
  * (here, [let module]) and decremented at its end (here, after the
  * module expression is parsed).No module definition is reported
  * while [!nesting > 0].
  *)
let nesting = ref 0

let reset ~rollback () =
  filter_first := rollback;
  nesting := 0

let enter_sub () =
  incr nesting

let leave_sub () =
  (if !nesting <= 0
   then failwith "Outline_utils.leave: invalid nesting");
  decr nesting

let emit_top c pos =
  (*prerr_endline "emit";*)
  if !nesting = 0 then begin
      if !filter_first < 0
      then failwith "Outline_utils.emit_top: invalid filter_first"
      else if !filter_first = 0
      then raise (Chunk (c,pos))
      else decr filter_first
    end
