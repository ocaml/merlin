open Ppxlib

let char_size = 8
let int_chars = (Sys.int_size + (char_size - 1)) / char_size

let int_array_of_string str =
  let length = String.length str in
  let size = (length + (int_chars - 1)) / int_chars in
  let array = Array.make size 0 in
  for i = 0 to size - 1 do
    let int = ref 0 in
    for j = 0 to int_chars - 1 do
      let index = i * int_chars + j in
      if index < length then begin
        let code = Char.code str.[index] in
        let shift = j * char_size in
        int := !int lor (code lsl shift);
      end;
    done;
    array.(i) <- !int;
  done;
  array

let print_loc ppf loc =
  let open Location in
  let open Lexing in
  let pos = loc.loc_end in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol - 1 in
  Format.fprintf ppf "%i:%i" line col

let enumerate ast =
  let folder = object
    inherit [int * Location.t list] Ast_traverse.fold

    method! longident_loc lid (nb, locs) = (nb + 1, lid.loc :: locs)
  end in
  folder#structure ast (0, [])

let parse_impl sourcefile =
  let ic = open_in sourcefile in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    Parse.implementation (Lexing.from_channel ic)
  )

let stop_server merlin =
  let command = merlin ^ " server stop-server" in
  match Sys.command command with
  | 255 -> ()
  | code -> failwith ("merlin exited with code " ^ string_of_int code)

let query_type merlin sourcefile location =
  let key =
    Format.asprintf "%s:%a"
      sourcefile print_loc location
  in
  let command =
    Format.asprintf
      "%s server locate -look-for ml -position '%a' -index 0 -filename %s < %s"
      merlin print_loc location sourcefile sourcefile
  in
  let result =
    let ic = Unix.open_process_in command in
    match Yojson.Basic.from_channel ic with
    | json -> begin
        match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> json
        | Unix.WEXITED code ->
            failwith ("merlin exited with code " ^ string_of_int code)
        | _ ->
            failwith "merlin closed unexpectedly"
      end
    | exception e ->
        ignore (Unix.close_process_in ic);
        raise e
  in
    match result with
    | `Assoc alist -> begin
        match List.assoc "class" alist with
        | `String "return" -> begin
            match List.assoc "value" alist with
            | `Assoc result -> begin
                match List.assoc "file" result, List.assoc "pos" result with
                | `String fn, `Assoc ([ "line", `Int ln; "col", `Int cn ]
                                     |[ "col", `Int cn; "line", `Int ln]) ->
                    Some (key, `Ok (fn, ln, cn))
                | exception Not_found | _ -> failwith "merlin gave bad output"
              end
            | `String s -> Some (key, `Error s)
            | _ | exception Not_found ->
                failwith "merlin gave bad output"
          end
        | `String _ -> None
        | _ | exception Not_found ->
            failwith "merlin gave bad output"
      end
    | _ ->
        failwith "merlin gave bad output"

let query_types merlin sourcefile locations =
  let rec loop acc locations =
    match locations with
    | [] -> List.rev acc
    | location :: rest ->
      let acc =
        match query_type merlin sourcefile location with
        | None -> acc
        | Some timing -> timing :: acc
      in
      loop acc rest
  in
  loop [] locations

let print_result ppf (key, res) =
  match res with
  | `Ok (fn, ln, cn) ->
    Format.fprintf ppf "%s: %s %d:%d" key fn ln cn
  | `Error s ->
    Format.fprintf ppf "%s: %s" key s

let print_results file results =
  let fn = Filename.chop_extension file ^ ".result" in
  let oc = open_out fn in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
    let ppf = Format.formatter_of_out_channel oc in
    Format.pp_print_list
      ~pp_sep:Format.pp_print_newline
      print_result ppf results
  )

let usage =
  "ocamlmerlin_tester MERLIN FILE"

let () =
  let args = ref [] in
  Arg.parse []
    (fun arg -> args := arg :: !args)
    usage;
  let merlin, file =
    match !args with
    | [file; merlin] -> merlin, file
    | _ ->
      Arg.usage [] usage;
      exit 1
  in
  match parse_impl file with
  | exception _ -> ()
  | ast ->
    let nb_locs, locs = enumerate ast in
    let seed = int_array_of_string file in
    let state = Random.State.make seed in
    let session =
      let keep_probability = nb_locs / 50 in
      List.filter (fun _ -> Random.State.int state keep_probability = 0) locs
    in
    let results = query_types merlin file session in
    stop_server merlin;
    print_results file results

