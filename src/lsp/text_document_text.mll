(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

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
