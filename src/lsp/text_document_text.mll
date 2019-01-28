{
  type position = Protocol.position = {
    line : int;
    character : int;
  }

  let position_in_range pos (range : Protocol.range) =
    if pos.line < range.start_.line || pos.line > range.end_.line
    then false
    else 
      let rel_start =
        if pos.line = range.start_.line
        then pos.character >= range.start_.character
        else true
      in
      let rel_end =
        if pos.line = range.end_.line
        then pos.character < range.end_.character
        else true
      in
      rel_start && rel_end

  let maybe_apply_change_with process pos buf range change =
    if pos.line = range.Protocol.start_.line && pos.character = range.start_.character then
    let c_lexbuf = Lexing.from_string change in
    let c_buf = Buffer.create (String.length change) in
    let () = process c_buf c_lexbuf in
    Buffer.add_buffer buf c_buf
}

let n = '\n'
let r = '\r'
let newline = r? n

rule normalize_line_endlings_rule buf = parse
  | eof { () }
  | newline {
      Buffer.add_char buf '\n';
      normalize_line_endlings_rule buf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      normalize_line_endlings_rule buf lexbuf
    }

  and apply_change_rule pos buf range change = parse
  | eof {
      maybe_apply_change_with
        normalize_line_endlings_rule
        pos buf range change
    }
  | newline {
      let () =
        maybe_apply_change_with
          normalize_line_endlings_rule
          pos buf range change
      in
      if not (position_in_range pos range) then Buffer.add_char buf '\n';
      let pos = {line = pos.line + 1; character = 0;} in
      apply_change_rule pos buf range change lexbuf
    }
  | _ as c {
      let () =
        maybe_apply_change_with
          normalize_line_endlings_rule
          pos buf range change
      in
      if not (position_in_range pos range) then Buffer.add_char buf c;
      let pos = {pos with character = pos.character + 1} in
      apply_change_rule pos buf range change lexbuf
    }

{

  let normalize_line_endings text =
    let lexbuf = Lexing.from_string text in
    let buf = Buffer.create (String.length text) in
    let () = normalize_line_endlings_rule buf lexbuf in
    Buffer.contents buf

  let apply_change text range change =
    let lexbuf = Lexing.from_string text in
    let buf = Buffer.create (String.length text) in
    let pos = {line = 0; character = 0;} in
    let () = apply_change_rule pos buf range change lexbuf in
    Buffer.contents buf

}
