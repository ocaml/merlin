open Misc
type position = Lexing.position

type 'a token = 'a * position * position

let wrap ~tokens f buf =
  match !tokens with
  | Zipper (_, _, ((t,s,c) :: _)) ->
    buf.Lexing.lex_start_p <- s;
    buf.Lexing.lex_curr_p <- c;
    tokens := Zipper.shift 1 !tokens;
    t
  | Zipper (_, _, []) ->
    let t = f buf in
    tokens := Zipper.insert Lexing.(t, buf.lex_start_p, buf.lex_curr_p) !tokens;
    t

let const token _ = token
