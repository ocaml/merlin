type offset = History.offset
type position = Lexing.position
let partial_definitions = ref []

type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated
  | Partial_definitions of (offset * offset) list
  | Exception of exn

exception Chunk of kind * position

let filter_first = ref 0
let nesting = ref 0

let reset ~rollback () =
  filter_first := rollback;
  partial_definitions := [];
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

(* Partial parsing looks like :
  [let a = b| in
  [let c = d| in
    e
  ]]  
  where [ = enter_partial  --> begin definition
        | = commit_partial --> definition completed, add it to scope
        ] = leave_partial  --> definition exits scope
*)

let get_offset, reset_get_offset =
  let dummy _ = 0 in
  let get = ref dummy in
  get, (fun () -> get := dummy)

let enter_partial p =
  let offset = !get_offset p in
  partial_definitions := (offset, []) :: !partial_definitions

let commit_partial p =
  let end_offset = !get_offset p in
  match !partial_definitions with
  | [] -> assert false
  | (start_offset,_) :: xs ->
    let (prev_offset, prev_defs), tail = match xs with
      | item :: tail -> item, tail
      | [] -> (0,[]), []
    in
    let def = (start_offset,end_offset) in 
    partial_definitions := (prev_offset, def :: prev_defs) :: tail

let leave_partial () =
  match !partial_definitions with
  | (start, def :: defs) :: tail ->
    partial_definitions := (start, defs) :: tail
  | _ -> ()
