let ( let> ) x f = x f

type pos = { line : int; col : int }
type range = { start : pos; end_ : pos }

let pos_of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc [ ("line", `Int line); ("col", `Int col) ]
  | `Assoc [ ("col", `Int col); ("line", `Int line) ] ->
    { line = line - 1; col }
  | _ -> failwith "wrong pos, expected an object with two fields"

let range_of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc [ ("start", pos_start); ("end", pos_end) ]
  | `Assoc [ ("end", pos_end); ("start", pos_start) ] ->
    let start = pos_of_json pos_start in
    let end_ = pos_of_json pos_end in
    { start; end_ }
  | _ -> failwith "wrong range, expected an object with two fields"

let extract range s =
  s
  |> Array.mapi (fun i s ->
      if i < range.start.line then None
      else if i = range.start.line && range.start.line = range.end_.line then
        let spaces = String.init range.start.col (fun _ -> ' ') in
        let extracted =
          String.sub s range.start.col (range.end_.col - range.start.col)
        in
        Some (spaces ^ extracted)
      else if i = range.start.line then
        let spaces = String.init range.start.col (fun _ -> ' ') in
        let extracted =
          String.sub s range.start.col (String.length s - range.start.col)
        in
        Some (spaces ^ extracted)
      else if i < range.end_.line then Some s
      else if i = range.end_.line then Some (String.sub s 0 range.end_.col)
      else None)
  |> Array.to_list |> List.filter_map Fun.id |> String.concat "\n"

let ranges_of_json (j : Yojson.Safe.t) =
  match j with
  | `List l -> List.map range_of_json l
  | _ -> failwith "wrong ranges, expected a list"

let () =
  let file =
    try Sys.argv.(1)
    with _ ->
      Format.eprintf
        "Usage: '%s <source file>' and then pass the ranges as json via stdin\n\
         %!"
        Sys.argv.(0);
      exit 1
  in
  let> ic = In_channel.with_open_bin file in
  let source = In_channel.input_all ic in
  let source = source |> String.split_on_char '\n' |> Array.of_list in
  let ranges =
    stdin |> In_channel.input_all |> Yojson.Safe.from_string |> ranges_of_json
  in
  ranges
  |> List.iteri @@ fun i range ->
     let extracted_range = extract range source in
     Format.printf "---------- Range %d ----------\n%s\n%!" i extracted_range
