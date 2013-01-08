type position = Lexing.position
type position_compare = position -> int
type 'a token = 'a * position * position

type stat = { first : position ; last : position }

type 'a t = { prev : 'a token list ; next : 'a token list ; stat : stat }

let zero_pos = Lexing.({ pos_bol = 0 ; pos_cnum = 0 ; pos_lnum = 0 ; pos_fname = "" })

let empty = { prev = [] ; next = [] ; stat = { first = zero_pos ; last = zero_pos } }

let wrap r f buf =
  match !r with
    | { prev ; next = (t,s,c as tok) :: ns } as history ->
        buf.Lexing.lex_start_p <- s;
        buf.Lexing.lex_curr_p <- c;
        r := { history with prev = tok :: prev ; next = ns };
        t
    | { prev ; stat } as history ->
        (if stat.last <> zero_pos then
            buf.Lexing.lex_curr_p <- stat.last);
        let t = f buf in
        let first =
          if stat.first = zero_pos
          then buf.Lexing.lex_start_p
          else stat.first
        in
        r := { history with
          prev = (t, buf.Lexing.lex_start_p, buf.Lexing.lex_curr_p) :: prev;
          stat = { first ; last = buf.Lexing.lex_curr_p } };
        t

let seek_forward prj cmp =
  let rec aux prev next =
    match next with
      | t :: next' when cmp (prj t) > 0 ->
          aux (t :: prev) next'
      | _ -> prev, next
  in
  aux

let seek_backward prj cmp =
  let rec aux prev next =
    match prev with
      | t :: prev' when cmp (prj t) < 0 ->
          aux prev' (t :: next)
      | _ -> prev, next
  in
  aux

let seek_pos prj cmp ({ prev ; next } as history) =
  let prev', next' =
    match prev, next with
      | (t :: prev'), next when cmp (prj t) < 0 ->
          seek_backward prj cmp prev' (t :: next)
      | prev, (t :: next') when cmp (prj t) > 0 ->
          seek_forward prj cmp (t :: prev) next
      | x -> x
  in
  { history with prev = prev' ; next = next' }

let seek_start cmp = seek_pos (fun (_,p,_) -> p) cmp
let seek cmp = seek_pos (fun (_,_,p) -> p) cmp
  
let first_pos { stat = { first } } = first
let last_pos { stat = { last } } = last
(*let next_pos = function
  | { next = (_,_,p) :: _ } -> p
  | _ -> zero_pos*)

let current_pos = function
  | { prev = (_,_,p) :: _ } -> p
  | _ -> zero_pos

let this_position p1 p2 =
  compare p1.Lexing.pos_cnum p2.Lexing.pos_cnum

let this_offset p1 p2 =
  compare p1 p2.Lexing.pos_cnum

(*let drop_next = function
  | { prev = (_,_,last) :: _ ; next ; stat = { first } } ->
      { prev ; next = [] ; stat = { first ; last } }, next
  | { prev = [] ; next } ->
      { prev = [] ; next = [] ; stat = empty.stat }, next*)

let split = function
  | { prev = [] ; next = _ } as a -> empty, a
  | { prev = _ ; next = [] } as a -> a, empty
  | { prev = (_,_,last') :: _ ;
      next = (_,first',_) :: _ ;
      stat = { first ; last }
    } as history ->
      { prev = history.prev ; next = [] ; stat = { first ; last = last' } },
      { prev = [] ; next = history.next ; stat = { first = first' ; last } }

let forward = function
  | { prev ; next = n :: ns } as history ->
      Some n, { history with prev = n :: prev ; next = ns }
  | history -> None, history

let backward = function
  | { prev = p :: ps ; next } as history ->
      Some p, { history with prev = ps ; next = p :: next }
  | history -> None, history

let insert (_,start,curr as tok) = function
  | { prev = [] ; next = [] } ->
      { prev = [tok] ; next = [] ; stat = { first = start ; last = curr } }
  | { prev ; next = [] } ->
      { prev = [tok] ; next = [] ; stat = { first = start ; last = curr } }
