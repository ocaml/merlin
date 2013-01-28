type pos = Lexing.position
type 'a t = { prev : 'a list ; next : 'a list ; pos : int }

let empty = { prev = [] ; next = [] ; pos = 0 }

let of_list next = { empty with next }

let split { prev ; next ; pos } =
  { prev ; next = [] ; pos }, { prev = [] ; next; pos = 0 }

let cutoff = function
  | { next = [] } as h -> h
  | h -> { h with next = [] }

let prev = function
  | { prev = p :: _ } -> Some p
  | _ -> None

let prevs { prev } = prev
 
let next = function
  | { next = n :: _ } -> Some n
  | _ -> None

let nexts { next } = next

type offset = int
let offset { pos } = pos
  
let move amount h =
  let rec shift count lx ly =
    match count, lx, ly with
      | n, (x :: xs), ys when n < 0 -> shift (succ n) xs (x :: ys)
      | n, xs, (y :: ys) when n > 0 -> shift (pred n) (y :: xs) ys
      | n, xs, ys -> n, xs, ys
  in
  let diff, prev, next = shift amount h.prev h.next in
  let moved = amount - diff in
  { prev ; next ; pos = h.pos + moved }
  
let seek_offset offset h =
  move (offset - h.pos) h
    

let forward = function
  | { prev ; next = n :: ns ; pos } ->
      Some (n, { prev = n :: prev ; next = ns ; pos = succ pos })
  | history -> None

let backward = function
  | { prev = p :: ps ; next ; pos } ->
      Some (p, { prev = ps ; next = p :: next ; pos = pred pos })
  | history -> None

let insert p { pos ; prev ; next } =
  { prev = p :: prev ; next ; pos = succ pos }

let remove = function
  | { prev = p :: ps ; next ; pos } ->
      Some (p, { prev = ps ; next ; pos = pred pos })
  | x -> None

let modify f = function
  | { prev = p :: ps ; next ; pos } ->
      { prev = (f p) :: ps ; next ; pos }
  | x -> x

let seek_forward cmp =
  let rec aux prev next pos =
    match next with
      | t :: next' when cmp t > 0 ->
          aux (t :: prev) next' (succ pos)
      | _ -> prev, next, pos
  in
  aux

let seek_backward cmp =
  let rec aux prev next pos =
    match prev with
      | t :: prev' when cmp t < 0 ->
          aux prev' (t :: next) (pred pos)
      | _ -> prev, next, pos
  in
  aux

let seek cmp { prev ; next ; pos } =
  let prev', next', pos' =
    match prev, next with
      | (t :: prev'), next when cmp t < 0 ->
          seek_backward cmp prev' (t :: next) (pred pos)
      | prev, (t :: next') when cmp t > 0 ->
          seek_forward cmp (t :: prev) next (succ pos)
      | _ -> prev, next, pos
  in
  { prev = prev' ; next = next' ; pos = pos' }

(* val wrap : ('a * pos * pos) t ref -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a) *)
type 'a loc = 'a * pos * pos

let wrap_lexer ?(filter=fun _-> true) ?bufpos r f buf =
  let t = match forward !r with
    | Some ((t,s,c), r') ->
        buf.Lexing.lex_start_p <- s;
        buf.Lexing.lex_curr_p <- c;
        r := r';
        t
    | None ->
        (match bufpos with
          | Some p -> buf.Lexing.lex_curr_p <- !p
          | None -> ());
        let t = f buf in
        if filter t then 
          r := insert (t,buf.Lexing.lex_start_p,buf.Lexing.lex_curr_p) !r;
        (match bufpos with
          | Some p -> p := buf.Lexing.lex_curr_p;
          | None -> ());
        t
  in
  t

let current_pos ?(default=Lexing.dummy_pos) hist =
  match prev hist with
    | Some (_,_,p) -> p
    | _ -> default

let seek_pos pos =
  seek (fun (_,_,p) -> compare pos.Lexing.pos_cnum p.Lexing.pos_cnum)

type 'a sync = (int * 'a) option

module Sync =
struct
  let origin = None
  
  let (>>=) = function
    | None   -> fun _ -> None
    | Some a -> fun f -> f a
  
  let at h =
    prev h >>= fun a -> Some (offset h, a)
  
  let item = function
    | None -> None
    | Some (_,a) -> Some a
  
  let rec nearest f ah bh =
    let point = prev bh >>= f in
    let found = point >>=
      fun (off,a) ->
      let ah' = seek_offset off ah in
      prev ah' >>= function
        | a' when a' == a -> Some (ah', bh)
        | _ -> backward bh >>= fun (_,bh') -> Some (nearest f ah' bh')
    in
    match found with
      | Some a -> a
      | None   -> seek_offset 0 ah, seek_offset 0 bh
  
  let rec rewind f ah bh =
    let point = prev bh >>= f in
    let found = point >>=
      fun (off,a) ->
      let ah' = if off <= offset ah
        then seek_offset off ah
        else ah
      in
      prev ah' >>= function
        | a' when a' == a -> Some (ah', bh)
        | _ -> backward bh >>= fun (_,bh') -> Some (rewind f ah' bh')
    in
    match found with
      | Some a -> a
      | None   -> seek_offset 0 ah, seek_offset 0 bh
  
  let right f ah bh =
    let off = offset ah in
    let rec loop bh =
      match forward bh with
        | Some (item,bh') ->
            let off' = match f item with
              | None -> 0
              | Some (off',_) -> off'
            in
            if off' < off
            then loop bh'
            else bh'
        | _ -> bh
    in
    match backward bh with
      | Some (_,bh') -> loop bh'
      | None -> loop bh
  
  let left f ah bh =
    let off =
      match prev bh >>= f with
        | None -> 0
        | Some (off,_) -> off
    in
    seek_offset off ah
end
