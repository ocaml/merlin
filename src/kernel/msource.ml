(* Merlin representation of a textual source code *)
open Std

type t = {
  text: string;
}

let dump t = `Assoc [
    "text"     , `String t.text;
  ]

let dump_short _ = `Assoc [
    "text"     , `String "...";
  ]

let print_position () = function
  | `Start -> "start"
  | `Offset o -> string_of_int o
  | `Logical (l,c) -> string_of_int l ^ ":" ^ string_of_int c
  | `End -> "end"

let make tr text =
  Trace.enter tr "Msource.make %S" text
    ~return:(Json.print dump)
  @@ fun _tr -> {text}

(* Position management *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

exception Found of int

let find_line line {text} =
  if line <= 0 then
    (Printf.ksprintf invalid_arg "Msource.find_line: invalid line number %d. Numbering starts from 1" line);
  if line = 1 then 0 else
    let line' = ref line in
    try
      for i = 0 to String.length text - 1 do
        if text.[i] = '\n' then begin
          decr line';
          if !line' = 1 then
            raise (Found i);
        end
      done;
      Logger.logf "source" "find_line"
        "line %d out of bounds (max = %d)" line (line - !line');
      String.length text
    with Found n ->
      n + 1

let find_offset ({text} as t) line col =
  assert (col >= 0);
  let offset = find_line line t in
  if col = 0 then offset else
    try
      for i = offset to min (offset + col) (String.length text) - 1 do
        if text.[i] = '\n' then begin
          Logger.logf "source" "find_offset"
            "%d:%d out of line bounds, line %d only has %d columns"
            line col line (i - offset);
          raise (Found i)
        end
      done;
      if (offset + col) > (String.length text) then begin
        Logger.logf "source" "find_offset"
          "%d:%d out of file bounds"
          line col
      end;
      offset + col
    with Found off -> off

let get_offset t = function
  | `Start -> `Offset 0
  | `Offset x ->
    assert (x >= 0);
    if x <= String.length t.text then
      (`Offset x)
    else begin
      Logger.logf "source" "get_offset"
        "offset %d out of bounds (size is %d)"
        x (String.length t.text);
      (`Offset (String.length t.text))
    end
  | `End ->
    `Offset (String.length t.text)
  | `Logical (line, col) ->
    `Offset (find_offset t line col)

let get_logical {text} = function
  | `Start -> `Logical (1, 0)
  | `Logical _ as p -> p
  | `Offset _ | `End as r ->
    let len = String.length text in
    let offset = match r with
      | `Offset x when x > len ->
        Logger.logf "source" "get_logical"
          "offset %d out of bounds (size is %d)" x len;
        len
      | `Offset x ->
        assert (x >= 0);
        x
      | `End -> len
    in
    let line = ref 1 in
    let cnum = ref 0 in
    for i = 0 to offset - 1 do
      if text.[i] = '\n' then begin
        incr line;
        cnum := i + 1;
      end;
    done;
    `Logical (!line, offset - !cnum)

let get_lexing_pos t ~filename pos =
  let `Offset o = get_offset t pos in
  let `Logical (line, col) = get_logical t pos in
  { Lexing.
    pos_fname = filename;
    pos_lnum = line;
    pos_bol  = o - col;
    pos_cnum = o;
  }

let get_offset tr t pos =
  Trace.enter tr "Msource.get_offset %a %a"
    (Json.print dump_short) t
    print_position pos
    ~return:(fun()->function (`Offset o) -> string_of_int o |_->"")
  @@ fun _tr -> get_offset t pos

let get_logical tr t pos =
  Trace.enter tr "Msource.get_logical %a %a"
    (Json.print dump_short) t
    print_position pos
    ~return:(fun()->function (`Logical (l,c)) -> sprintf "%d:%d" l c |_->"")
  @@ fun _tr -> get_logical t pos

let get_lexing_pos tr t ~filename pos =
  Trace.enter tr "Msource.lexing_pos %a ~filename:%s %a"
    (Json.print dump_short) t
    filename
    print_position pos
    ~return:Lexing.print_position
  @@ fun _tr -> get_lexing_pos t ~filename pos

let substitute tr t starting ending text =
  Trace.enter tr "Msource.substitute %a %a %a %S"
    (Json.print dump_short) t
    print_position starting
    (fun () -> function
       | #position as p -> print_position () p
       | `Length n -> "length " ^ string_of_int n
    ) ending
    text
    ~return:(Json.print dump_short)
  @@ fun tr ->
  let len = String.length t.text in
  let `Offset starting = get_offset tr t starting in
  let `Offset ending = match ending with
    | `End -> `Offset len
    | `Length l ->
      if starting + l <= len then
        `Offset (starting + l)
      else begin
        Logger.logf "source" "substitute"
          "offset %d + length %d out of bounds (size is %d)"
          starting l len;
        `Offset len
      end
    | #position as p -> get_offset tr t p
  in
  if ending < starting then
    invalid_arg "Source.substitute: ending < starting";
  let text =
    String.sub t.text ~pos:0 ~len:starting ^
    text ^
    String.sub t.text ~pos:ending ~len:(len - ending)
  in
  {text}

(* Accessing content *)

let text t = t.text
