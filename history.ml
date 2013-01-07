type position = Lexing.position
type 'a token = 'a * position * position

type stat = { first : position ; last : position }

type 'a t = { prev : 'a token list ; next : 'a token list ; stat : stat }

let empty = { prev = [] ; next = [] ; stat = { first = Lexing.dummy_pos ; last = Lexing.dummy_pos } }

let wrap r f buf =
  match !r with
    | { next = (t,s,c) :: ns } as history ->
        buf.Lexing.lex_start_p <- s;
        buf.Lexing.lex_curr_p <- c;
        r := { history with next = ns };
        t
    | { prev ; stat } as history ->
        (if stat.last <> Lexing.dummy_pos then
            buf.Lexing.lex_curr_p <- stat.last);
        let t = f buf in
        let first =
          if stat.first = Lexing.dummy_pos
          then buf.Lexing.lex_start_p
          else stat.first
        in
        r := { history with
          prev = (t, buf.Lexing.lex_start_p, buf.Lexing.lex_curr_p) :: prev;
          stat = { first ; last = buf.Lexing.lex_curr_p } };
        t

let compare_pos p1 p2 = compare p1.Lexing.pos_cnum p2.Lexing.pos_cnum

let seek_forward f cnum =
  let rec aux prev next =
    match next with
      | t :: next' when cnum > f t ->
          aux (t :: prev) next'
      | _ -> prev, next
  in
  aux

let seek_backward f cnum =
  let rec aux prev next =
    match prev with
      | t :: prev' when cnum < f t ->
          aux prev' (t :: next)
      | _ -> prev, next
  in
  aux

let seek_pos f cnum ({ prev ; next } as history) =
  let prev', next' =
    match prev, next with
      | (t :: prev'), next when cnum < f t ->
          seek_backward f cnum prev' (t :: next)
      | prev, (t :: next') when cnum > f t ->
          seek_forward f cnum (t :: prev) next
  in
  { history with prev = prev' ; next = next' }

let seek_start { Lexing.pos_cnum } = seek_pos (fun (_,p,_) -> p) pos_cnum
let seek_curr  { Lexing.pos_cnum } = seek_pos (fun (_,_,p) -> p) pos_cnum

let first_pos { stat = { first } } = first
let last_pos { stat = { last } } = last
let next_pos = function
  | { next = (_,_,p) } -> p
  | _ -> Lexing.dummy_pos

let drop_next = function
  | { prev = (_,_,last) :: _ ; next ; stat = { first } } ->
      { prev ; next = [] ; stat = { first ; last } }, next
  | { prev = [] ; next } ->
      { prev = [] ; next = [] ; stat = empty.stat }, next
