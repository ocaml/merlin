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

let pos_to_json pos =
  Lexing.(`Assoc ["line", `Int pos.pos_lnum;
                  "col", `Int (pos.pos_cnum - pos.pos_bol)])
                  (*"offset", `Int pos.pos_cnum])*)
let pos_of_json = function
  | `Assoc props ->
    (*try match List.assoc "offset" props with
      | `Int i -> failwith "FIXME: offsets are computed incorrectly"; `Offset i
      | _ -> failwith "Incorrect position"
    with Not_found ->*)
    begin try match List.assoc "line" props, List.assoc "col" props with
      | `Int line, `Int col -> `Line (line, col)
      | _ -> failwith "Incorrect position"
    with Not_found -> failwith "Incorrect position"
    end
  | _ -> failwith "Incorrect position"

let ppf_to_string () =
  let b = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer b in
  ppf,
  (fun () ->
    Format.pp_print_flush ppf ();
    Buffer.contents b)

(* Partial parsing looks like :
  [let a = b| in
  [let c = d| in
    e
  ]]  
  where [ = enter_partial
        | = leave_partial
        ] = commit_partial
*)

let get_offset, reset_get_offset =
  let dummy _ = 0 in
  let get = ref dummy in
  get, (fun () -> get := dummy)

let enter_partial p =
  let offset = !get_offset p in
  Printf.eprintf "enter_partial %d\n%!" offset;
  partial_definitions := (offset, []) :: !partial_definitions

let leave_partial p =
  let end_offset = !get_offset p in
  Printf.eprintf "leave_partial %d\n%!" end_offset;
  match !partial_definitions with
  | [] -> assert false
  | (start_offset,_) :: xs ->
    let (prev_offset, prev_defs), tail = match xs with
      | item :: tail -> item, tail
      | [] -> (0,[]), []
    in
    let def = (start_offset,end_offset) in 
    partial_definitions := (prev_offset, def :: prev_defs) :: tail

let commit_partial p =
  prerr_endline "commit_partial";
  match !partial_definitions with
  | (start, def :: defs) :: tail ->
    partial_definitions := (start, defs) :: tail
  | _ -> ()
