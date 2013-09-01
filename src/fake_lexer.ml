open Misc
type position = Lexing.position

type 'a token = 'a * position * position

let wrap ~tokens ?bufpos f buf =
  match !tokens with
  | Zipper (_, _, ((t,s,c) :: _)) ->
    buf.Lexing.lex_start_p <- s;
    buf.Lexing.lex_curr_p <- c;
    tokens := Zipper.shift 1 !tokens;
    t
  | Zipper (_, _, []) ->
    Misc.may begin fun {contents = p} ->
      buf.Lexing.lex_abs_pos <- Lexing.(p.pos_cnum - buf.lex_curr_pos);
      buf.Lexing.lex_curr_p <- p
    end bufpos;
    let t = f buf in
    tokens := Zipper.insert Lexing.(t, buf.lex_start_p, buf.lex_curr_p) !tokens;
    Misc.may begin fun pos ->
      pos := buf.Lexing.lex_curr_p
    end bufpos;
    t

let const token _ = token 
