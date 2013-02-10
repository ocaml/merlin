let format ~valid ~where ?loc msg =
  let content = ["valid", `Bool valid; "message", `String msg] in
  let content =
    match loc with
      | None -> content
      | Some loc ->
        ("start", Protocol.pos_to_json loc.Location.loc_start) ::
        ("end", Protocol.pos_to_json loc.Location.loc_end) ::
        content
  in
  let content = ("type", `String where) :: content in
  `Assoc content

let strict_to_json = function
  | Typecore.Error (loc, e) ->
      let ppf, to_string = Misc.ppf_to_string () in
      Typecore.report_error ppf e;
      Some (format ~valid:true ~where:"type" ~loc (to_string ()))
  | Typetexp.Error (loc, e) ->
      let ppf, to_string = Misc.ppf_to_string () in
      Typetexp.report_error ppf e;
      Some (format ~valid:true ~where:"type" ~loc (to_string ()))
  | Typedecl.Error (loc, e) ->
      let ppf, to_string = Misc.ppf_to_string () in
      Typedecl.report_error ppf e;
      Some (format ~valid:true ~where:"type" ~loc (to_string ()))
  | Typemod.Error (loc, e) ->
      let ppf, to_string = Misc.ppf_to_string () in
      Typemod.report_error ppf e;
      Some (format ~valid:true ~where:"type" ~loc (to_string ()))
  | Env.Error e ->
      let ppf, to_string = Misc.ppf_to_string () in
      Env.report_error ppf e;
      Some (format ~valid:true ~where:"env" (to_string ()))
  | Syntaxerr.Error e ->
      let ppf, to_string = Misc.ppf_to_string () in
      Syntaxerr.report_error ppf e;
      let loc = match e with
        | Syntaxerr.Unclosed (loc,_,loc',_) ->
            Location.({ loc_start = loc.loc_start;
                        loc_end = loc'.loc_end;
                        loc_ghost = false;
                      })
        | Syntaxerr.Applicative_path loc -> loc
        | Syntaxerr.Variable_in_scope (loc,_) -> loc
        | Syntaxerr.Other loc -> loc
      in
      Some (format ~valid:true ~where:"parser" ~loc (to_string ()))
  | Outline.Parse_error loc ->
      Some (format ~valid:true ~where:"parser" ~loc "Parse error")
  | Chunk.Warning (loc, msg) ->
      Some (format ~valid:true ~where:"warning" ~loc msg)
  | Chunk.Malformed_module loc ->
      Some (format ~valid:true ~where:"warning" ~loc "Malformed module")
  | exn -> None

let to_json exn = match strict_to_json exn with
  | Some j -> j
  | None ->
    let zero = { Lexing. pos_fname="" ; pos_bol=0 ; pos_lnum=1 ; pos_cnum=0 } in
    let loc = { Location. loc_start=zero ; loc_end=zero ; loc_ghost=true } in
    format ~valid:false ~where:"unknown" ~loc (Printexc.to_string exn)

let rec list_filter_map f = function
  | [] -> []
  | x :: xs -> match f x with
      | Some x' -> x' ::  list_filter_map f xs
      | None    -> list_filter_map f xs

let strict_to_jsons list = list_filter_map strict_to_json list
let to_jsons list = List.map to_json list

let _ = Protocol.error_catcher := strict_to_json
